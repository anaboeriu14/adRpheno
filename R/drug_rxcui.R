# Base URL for the National Library of Medicine RxNav REST API.
# Centralized so that endpoint changes only need a single update.
.RXNAV_BASE_URL <- "https://rxnav.nlm.nih.gov/REST"


#' Get RxCUI for a single medication
#'
#' Queries the RxNorm API using "exact then normalized" matching, which
#' handles abbreviations (e.g. "hctz" -> "hydrochlorothiazide") without
#' false positives.
#'
#' Note: this is a thin single-call wrapper with no caching. For batch
#' lookups, use [add_rxcuis()], which caches results to disk.
#'
#' @param med_name Medication name (ideally an ingredient name).
#' @param timeout API timeout in seconds (default: 10).
#'
#' @return RxCUI as a character scalar, or `NA_character_` if the lookup
#'   fails or returns no match.
#' @export
#'
#' @examples
#' \dontrun{
#' get_single_rxcui("atorvastatin")
#' get_single_rxcui("hctz")
#' }
get_single_rxcui <- function(med_name, timeout = 10) {
  adRutils::validate_args(
    med_name = adRutils::is_string(),
    timeout  = adRutils::is_number(positive = TRUE)
  )

  if (trimws(med_name) == "") return(NA_character_)

  url <- .build_rxcui_url(med_name)

  tryCatch({
    response <- httr::GET(url, httr::timeout(timeout))
    if (httr::status_code(response) != 200) return(NA_character_)
    .parse_rxcui_response(response)
  }, error = function(e) {
    cli::cli_alert_warning("RxCUI lookup failed for {.val {med_name}}: {e$message}")
    NA_character_
  })
}


#' Add RxCUI codes to medication dataframe
#'
#' Looks up RxCUI codes for a column of medication names. Results are cached
#' to disk so reruns and overlapping queries are cheap. Medication names
#' should be pre-cleaned (ingredient names, no dosages) for best match rates.
#'
#' @param dataf Data frame with medication names.
#' @param med_column Column containing medication names.
#' @param rxcui_column Name for the new RxCUI column (default: `"rxcui"`).
#'   Overwritten with a warning if it already exists.
#' @param batch_size Medications per batch (default: 500).
#' @param save_freq Save cache every N items (default: 500).
#' @param cache_dir Cache directory (default: `"cache"`).
#' @param max_age_days Cache expiration in days (default: 30).
#' @param retry_count Retry attempts (default: 3).
#' @param batch_delay Seconds between batches (default: 0.5).
#'
#' @return The input data frame with `rxcui_column` added.
#' @export
#'
#' @examples
#' \dontrun{
#' meds <- data.frame(med_clean = c("atorvastatin", "metformin", "lisinopril"))
#' add_rxcuis(meds, med_column = "med_clean")
#' }
add_rxcuis <- function(dataf,
                       med_column,
                       rxcui_column = "rxcui",
                       batch_size = 500,
                       save_freq = 500,
                       cache_dir = "cache",
                       max_age_days = 30,
                       retry_count = 3,
                       batch_delay = 0.5) {

  adRutils::validate_args(
    data         = dataf,
    columns      = med_column,
    med_column   = adRutils::is_string(),
    rxcui_column = adRutils::is_string(),
    batch_size   = adRutils::is_number(positive = TRUE),
    save_freq    = adRutils::is_number(positive = TRUE),
    cache_dir    = adRutils::is_string(),
    max_age_days = adRutils::is_number(positive = TRUE),
    retry_count  = adRutils::is_number(positive = TRUE),
    batch_delay  = adRutils::is_number(min = 0)
  )

  if (rxcui_column %in% names(dataf)) {
    cli::cli_alert_warning("Column '{rxcui_column}' will be overwritten")
  }

  start_time <- Sys.time()
  unique_meds <- .get_unique_non_na(dataf[[med_column]])

  if (length(unique_meds) == 0L) {
    dataf[[rxcui_column]] <- NA_character_
    return(dataf)
  }

  results <- .process_batch(
    items         = unique_meds,
    api_function  = get_single_rxcui,
    batch_size    = batch_size,
    save_freq     = save_freq,
    cache_name    = "rxcui_cache",
    cache_dir     = cache_dir,
    max_age_days  = max_age_days,
    retry_count   = retry_count,
    batch_delay   = batch_delay,
    process_type  = "medications"
  )

  # RxCUI lookups are scalar: one code per medication.
  dataf[[rxcui_column]] <- vapply(
    dataf[[med_column]],
    function(med) {
      if (is.na(med) || trimws(med) == "") return(NA_character_)
      val <- results[[med]]
      if (is.null(val) || length(val) == 0L) NA_character_ else as.character(val[1])
    },
    character(1),
    USE.NAMES = FALSE
  )

  .report_pipeline_results(
    found      = sum(!is.na(dataf[[rxcui_column]])),
    total      = sum(!is.na(dataf[[med_column]]) & trimws(as.character(dataf[[med_column]])) != ""),
    label      = "RxCUI",
    start_time = start_time
  )

  dataf
}


#' Build an RxNav REST URL for a name lookup
#' @keywords internal
#' @noRd
.build_rxcui_url <- function(med_name) {
  encoded <- utils::URLencode(trimws(med_name), reserved = TRUE)
  # search=2: exact match, then normalized match
  paste0(.RXNAV_BASE_URL, "/rxcui.json?name=", encoded, "&search=2")
}

#' Parse an RxNav rxcui JSON response into a single RxCUI
#' @keywords internal
#' @noRd
.parse_rxcui_response <- function(response) {
  json <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  rxcui <- json$idGroup$rxnormId[1]
  if (is.null(rxcui) || is.na(rxcui)) NA_character_ else as.character(rxcui)
}


#' Extract unique, non-NA, non-blank values from a vector
#'
#' Shared between `add_rxcuis` and `add_atc_classification`.
#' @keywords internal
#' @noRd
.get_unique_non_na <- function(x) {
  if (length(x) == 0L) return(character())
  trimmed <- trimws(as.character(x))
  unique(trimmed[!is.na(trimmed) & trimmed != ""])
}


#' Emit a "found X/Y in Z min" success message
#' @keywords internal
#' @noRd
.report_pipeline_results <- function(found, total, label, start_time) {
  pct <- if (total > 0) round(100 * found / total, 1) else 0
  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
  cli::cli_alert_success("{label}: {found}/{total} ({pct}%) in {elapsed} min")
}
