#' Get RxCUI for a single medication
#'
#' Queries RxNorm API using "exact then normalized" matching, which handles
#' abbreviations (e.g., "hctz" → "hydrochlorothiazide") without false positives.
#'
#' @param med_name Medication name (ideally an ingredient name)
#' @param retry_count Retry attempts on failure (default: 3)
#' @param timeout API timeout in seconds (default: 10)
#'
#' @return RxCUI as character, or NA_character_ if not found
#' @export
#'
#' @examples
#' \dontrun{
#' get_single_rxcui("atorvastatin")
#' get_single_rxcui("hctz")
#' }
get_single_rxcui <- function(med_name, retry_count = 3, timeout = 10) {

  if (!is.character(med_name) || length(med_name) != 1) {
    cli::cli_abort("{.arg med_name} must be a single character string")
  }
  if (is.na(med_name) || trimws(med_name) == "") {
    return(NA_character_)
  }

  url <- .build_rxcui_url(med_name)

  for (attempt in seq_len(retry_count)) {
    result <- tryCatch({
      response <- httr::GET(url, httr::timeout(timeout))
      if (httr::status_code(response) == 200) {
        .parse_rxcui_response(response)
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    if (!is.na(result)) return(result)
    if (attempt < retry_count) Sys.sleep(0.5)
  }

  NA_character_
}


#' Add RxCUI codes to medication dataframe
#'
#' Looks up RxCUI codes for a column of medication names with caching.
#' Medication names should be pre-cleaned (ingredient names, no dosages).
#'
#' @param dataf Data frame with medication names
#' @param med_column Column containing medication names
#' @param rxcui_column Name for new RxCUI column (default: "rxcui")
#' @param batch_size Medications per batch (default: 200)
#' @param save_freq Save cache every N items (default: 50)
#' @param cache_dir Cache directory (default: "cache")
#' @param max_age_days Cache expiration in days (default: 30)
#' @param retry_count Retry attempts (default: 3)
#' @param batch_delay Seconds between batches (default: 2)
#'
#' @return Data frame with RxCUI column added
#' @export
#'
#' @examples
#' \dontrun{
#' testMeds <- data.frame(med_clean = c("atorvastatin", "metformin", "lisinopril"))
#' testMeds_with_rxcui <- add_rxcuis(testMeds, med_column = "med_clean")
#' }
add_rxcuis <- function(dataf,
                       med_column,
                       rxcui_column = "rxcui",
                       batch_size = 200,
                       save_freq = 50,
                       cache_dir = "cache",
                       max_age_days = 30,
                       retry_count = 3,
                       batch_delay = 2) {

  start_time <- Sys.time()

  # Validate
  .validate_rxcui_inputs(dataf, med_column, rxcui_column)

  # Get unique medications
  med_info <- .get_unique_medications(dataf, med_column)

  if (med_info$count == 0) {
    dataf[[rxcui_column]] <- NA_character_
    return(dataf)
  }

  # Process
  rxcui_results <- .process_batch(
    items = med_info$unique_meds,
    api_function = get_single_rxcui,
    batch_size = batch_size,
    save_freq = save_freq,
    cache_name = "rxcui_cache",
    cache_dir = cache_dir,
    max_age_days = max_age_days,
    retry_count = retry_count,
    batch_delay = batch_delay,
    process_type = "medications"
  )

  # Map results
  dataf[[rxcui_column]] <- rxcui_results[dataf[[med_column]]]

  # Report
  .report_rxcui_results(dataf, rxcui_column, med_column, start_time)

  return(dataf)
}

#' @keywords internal
#' @noRd
.build_rxcui_url <- function(med_name) {
  encoded <- utils::URLencode(trimws(med_name), reserved = TRUE)
  paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?name=", encoded, "&search=2")
}


#' @keywords internal
#' @noRd
.parse_rxcui_response <- function(response) {
  json <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  rxcui <- json$idGroup$rxnormId[1]
  if (is.null(rxcui) || is.na(rxcui)) NA_character_ else as.character(rxcui)
}


#' @keywords internal
#' @noRd
.validate_rxcui_inputs <- function(dataf, med_column, rxcui_column) {
  if (!is.data.frame(dataf) || nrow(dataf) == 0) {
    cli::cli_abort("Input must be a non-empty data frame")
  }
  if (!med_column %in% names(dataf)) {
    cli::cli_abort("Column '{med_column}' not found")
  }

  if (rxcui_column %in% names(dataf)) {
    cli::cli_alert_warning("Column '{rxcui_column}' will be overwritten")
  }
}


#' @keywords internal
#' @noRd
.get_unique_medications <- function(dataf, med_column) {
  meds <- dataf[[med_column]]
  valid <- meds[!is.na(meds) & trimws(as.character(meds)) != ""]
  unique_meds <- unique(as.character(valid))

  list(unique_meds = unique_meds, count = length(unique_meds))
}


#' @keywords internal
#' @noRd
.report_rxcui_results <- function(dataf, rxcui_column, med_column, start_time) {
  found <- sum(!is.na(dataf[[rxcui_column]]))
  valid <- sum(!is.na(dataf[[med_column]]) & trimws(dataf[[med_column]]) != "")
  pct <- if (valid > 0) round(100 * found / valid, 1) else 0
  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)

  cli::cli_alert_success("RxCUI: {found}/{valid} ({pct}%) in {elapsed} min")
}
