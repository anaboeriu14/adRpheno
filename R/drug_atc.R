# ATC level structure (WHO ATC/DDD standard):
#   first  = anatomical main group     (1 char,  e.g. "C")
#   second = therapeutic subgroup      (3 chars, e.g. "C10")
#   third  = pharmacological subgroup  (4 chars, e.g. "C10A")
#   fourth = chemical subgroup         (5 chars, e.g. "C10AA")
# RxClass returns the 5th-level ATC code (the substance, e.g. "C10AA05").
# Higher-level codes are derived by truncation; their human-readable names
# are then looked up via a second RxClass call.
.ATC_LEVEL_WIDTHS <- c(first = 1L, second = 3L, third = 4L, fourth = 5L)


#' Add ATC classifications to medications
#'
#' Retrieves WHO ATC classifications for medications using RxCUI codes via
#' direct calls to the National Library of Medicine RxClass REST API.
#' Results are cached to disk so reruns and overlapping queries are cheap.
#'
#' For drugs with multiple ATC classifications, the default behavior
#' (`as_list_column = FALSE`) collapses them into a single semicolon-separated
#' string. Setting `as_list_column = TRUE` instead stores the per-row
#' classifications as a list-column, which preserves multi-value structure
#' for downstream tidyverse work (e.g. [tidyr::unnest_longer()]).
#'
#' @param dataf Data frame containing RxCUI values.
#' @param rxcui_col Column with RxCUI values (default: `"rxcui"`).
#' @param new_col_name Name for the new column. If `NULL` (default), an
#'   auto-generated name based on `atc_level` is used (e.g. `"atc2_class"`).
#' @param as_list_column Logical. If `TRUE`, the output column is a
#'   list-column where each entry is a character vector of ATC classes for
#'   that drug. If `FALSE` (default), multiple classes are joined by `"; "`.
#' @param atc_level ATC level: `"first"`, `"second"` (default), `"third"`,
#'   or `"fourth"`.
#' @param batch_size Items per batch (default: 500).
#' @param save_freq Save cache every N items (default: 500).
#' @param cache_dir Cache directory (default: `"cache"`).
#' @param max_age_days Cache expiration in days (default: 30).
#' @param retry_count Retry attempts (default: 3).
#' @param batch_delay Seconds between batches (default: 0.5).
#'
#' @return The input data frame with the ATC classification column added.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(med   = c("atorvastatin", "metformin"),
#'                  rxcui = c("83367", "6809"))
#'
#' # Default: collapsed string
#' add_atc_classification(df)
#'
#' # List-column for tidyverse-style downstream work
#' add_atc_classification(df, as_list_column = TRUE)
#'
#' # Different level
#' add_atc_classification(df, atc_level = "third")
#' }
add_atc_classification <- function(dataf,
                                   rxcui_col = "rxcui",
                                   new_col_name = NULL,
                                   as_list_column = FALSE,
                                   atc_level = "second",
                                   batch_size = 500,
                                   save_freq = 500,
                                   cache_dir = "cache",
                                   max_age_days = 30,
                                   retry_count = 3,
                                   batch_delay = 0.5) {

  # Resolve auto-generated name BEFORE validation so the
  # "already exists" check operates on the actual output name.
  level_num <- .atc_level_to_num(atc_level)
  if (is.null(new_col_name)) {
    new_col_name <- paste0("atc", level_num, "_class")
  }

  adRutils::validate_args(
    data           = dataf,
    columns        = rxcui_col,
    rxcui_col      = adRutils::is_string(),
    new_col_name   = adRutils::is_string(),
    as_list_column = adRutils::is_flag(),
    atc_level      = adRutils::is_one_of(c("first", "second", "third", "fourth")),
    batch_size     = adRutils::is_number(positive = TRUE),
    save_freq      = adRutils::is_number(positive = TRUE),
    cache_dir      = adRutils::is_string(),
    max_age_days   = adRutils::is_number(positive = TRUE),
    retry_count    = adRutils::is_number(positive = TRUE),
    batch_delay    = adRutils::is_number(min = 0)
  )

  if (new_col_name %in% names(dataf)) {
    cli::cli_alert_warning("Column '{new_col_name}' will be overwritten")
  }

  start_time <- Sys.time()
  unique_rxcuis <- .get_unique_non_na(dataf[[rxcui_col]])

  if (length(unique_rxcuis) == 0L) {
    dataf[[new_col_name]] <- if (as_list_column) {
      replicate(nrow(dataf), character(0), simplify = FALSE)
    } else {
      NA_character_
    }
    return(dataf)
  }

  cache_name <- paste0("atc", level_num, "_cache")

  results <- .process_batch(
    items         = unique_rxcuis,
    api_function  = function(rxcui) .get_atc_for_rxcui(rxcui, atc_level),
    batch_size    = batch_size,
    save_freq     = save_freq,
    cache_name    = cache_name,
    cache_dir     = cache_dir,
    max_age_days  = max_age_days,
    retry_count   = retry_count,
    batch_delay   = batch_delay,
    process_type  = paste0("ATC", level_num, " classifications")
  )

  result_df <- if (as_list_column) {
    .apply_atc_list_column(dataf, rxcui_col, new_col_name, results)
  } else {
    .apply_atc_collapsed(dataf, rxcui_col, new_col_name, results)
  }

  .report_pipeline_results(
    found      = .count_atc_found(result_df, new_col_name, as_list_column),
    total      = nrow(result_df),
    label      = paste0("ATC", level_num),
    start_time = start_time
  )

  result_df
}


# ---- ATC API ---------------------------------------------------------------

#' Look up ATC class(es) for a single RxCUI
#'
#' Two-step query against the RxClass REST API:
#'   1. `byRxcui.json?rxcui={cui}&relaSource=ATCPROD` -> raw 5th-level ATC
#'      codes (e.g. `"C10AA05"`).
#'   2. If `query_atc` is one of `"first"`/`"second"`/`"third"`/`"fourth"`,
#'      truncate each ATC code to the appropriate width and call
#'      `byId.json?classId={truncated}` to get the human-readable class name
#'      (e.g. `"hmg coa reductase inhibitors"`).
#'
#' Returns a character vector of class identifiers/names. Returns `NA` on
#' failure (HTTP error, no match, or any parse problem). Errors are
#' propagated as `NA` rather than thrown so the batch processor's retry
#' layer can decide whether to retry based on `NULL` returns vs throws --
#' i.e. this function intentionally does not throw on transient failures.
#'
#' @keywords internal
#' @noRd
.get_atc_for_rxcui <- function(rxcui, query_atc = "second", timeout = 10) {
  raw_atcs <- .fetch_raw_atc_for_rxcui(rxcui, timeout)
  if (is.null(raw_atcs) || all(is.na(raw_atcs))) return(NA_character_)

  if (query_atc == "none") return(raw_atcs)

  width <- .ATC_LEVEL_WIDTHS[[query_atc]]
  truncated <- unique(substr(raw_atcs, 1L, width))
  truncated <- truncated[nchar(truncated) == width]
  if (length(truncated) == 0L) return(NA_character_)

  names <- vapply(truncated,
                  function(code) .fetch_atc_class_name(code, timeout),
                  character(1), USE.NAMES = FALSE)
  unique(names[!is.na(names)])
}


#' Fetch the raw 5th-level ATC code(s) associated with an RxCUI
#'
#' Uses `relaSource = ATCPROD`. Returns a character vector of `classId`
#' values (e.g. `c("C10AA05")`), `NA_character_` on miss, or `NULL` on
#' transport failure (so the caller / retry layer can distinguish).
#' @keywords internal
#' @noRd
.fetch_raw_atc_for_rxcui <- function(rxcui, timeout) {
  url <- paste0(.RXNAV_BASE_URL,
                "/rxclass/class/byRxcui.json?rxcui=", utils::URLencode(as.character(rxcui)),
                "&relaSource=ATCPROD")

  response <- tryCatch(
    httr::GET(url, httr::timeout(timeout)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)

  json <- tryCatch(
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(json)) return(NULL)

  drug_info <- json$rxclassDrugInfoList$rxclassDrugInfo
  if (is.null(drug_info) || length(drug_info) == 0L) return(NA_character_)

  ids <- vapply(drug_info, function(entry) {
    cls <- entry$rxclassMinConceptItem$classId
    if (is.null(cls)) NA_character_ else as.character(cls)
  }, character(1))

  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  if (length(ids) == 0L) NA_character_ else ids
}


#' Look up the human-readable class name for an ATC code at any level
#'
#' Returns the `className` from `byId.json`. Returns `NA_character_` on any
#' failure or miss.
#' @keywords internal
#' @noRd
.fetch_atc_class_name <- function(atc_code, timeout) {
  url <- paste0(.RXNAV_BASE_URL,
                "/rxclass/class/byId.json?classId=", utils::URLencode(atc_code))

  response <- tryCatch(
    httr::GET(url, httr::timeout(timeout)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NA_character_)

  json <- tryCatch(
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(json)) return(NA_character_)

  concepts <- json$rxclassMinConceptList$rxclassMinConcept
  if (is.null(concepts) || length(concepts) == 0L) return(NA_character_)

  name <- concepts[[1]]$className
  if (is.null(name)) NA_character_ else tolower(as.character(name))
}


# ---- internal helpers ------------------------------------------------------

#' Map an `atc_level` string to its numeric tier
#' @keywords internal
#' @noRd
.atc_level_to_num <- function(level) {
  switch(level,
         "first"  = "1",
         "second" = "2",
         "third"  = "3",
         "fourth" = "4",
         "2")
}

#' Apply ATC results as a single semicolon-collapsed string column
#' @keywords internal
#' @noRd
.apply_atc_collapsed <- function(dataf, rxcui_col, new_col_name, results) {
  dataf[[new_col_name]] <- vapply(dataf[[rxcui_col]], function(rxcui) {
    if (is.na(rxcui)) return(NA_character_)
    atc <- results[[as.character(rxcui)]]
    if (is.null(atc) || all(is.na(atc))) return(NA_character_)
    if (length(atc) > 1L) paste(atc, collapse = "; ") else as.character(atc)
  }, character(1), USE.NAMES = FALSE)

  dataf
}

#' Apply ATC results as a list-column (one character vector per row)
#' @keywords internal
#' @noRd
.apply_atc_list_column <- function(dataf, rxcui_col, new_col_name, results) {
  dataf[[new_col_name]] <- lapply(dataf[[rxcui_col]], function(rxcui) {
    if (is.na(rxcui)) return(NA_character_)
    atc <- results[[as.character(rxcui)]]
    if (is.null(atc) || all(is.na(atc))) return(NA_character_)
    as.character(atc)
  })

  dataf
}

#' Count rows with at least one ATC classification, regardless of column shape
#' @keywords internal
#' @noRd
.count_atc_found <- function(dataf, col, as_list_column) {
  values <- dataf[[col]]
  if (as_list_column) {
    sum(vapply(values, function(v) {
      length(v) > 0L && !all(is.na(v))
    }, logical(1)))
  } else {
    sum(!is.na(values))
  }
}
