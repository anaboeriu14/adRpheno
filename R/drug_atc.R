#' Add ATC classifications to medications
#'
#' Retrieves WHO ATC classifications for medications using RxCUI codes.
#'
#' @param dataf Data frame containing RxCUI values
#' @param rxcui_col Column with RxCUI values (default: "rxcui")
#' @param new_col_name Name for new column (default: auto-generated, e.g., "atc2_class")
#' @param unnest Create separate rows for multiple classes (default: FALSE)
#' @param atc_level ATC level: "first", "second" (default), "third", or "fourth"
#' @param batch_size Items per batch (default: 500)
#' @param save_freq Save cache every N items (default: 500)
#' @param cache_dir Cache directory (default: "cache")
#' @param max_age_days Cache expiration in days (default: 30)
#' @param retry_count Retry attempts (default: 3)
#' @param batch_delay Seconds between batches (default: 0.5)
#'
#' @return Data frame with ATC classification column added
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(med = c("atorvastatin", "metformin"),
#'                  rxcui = c("83367", "6809"))
#'
#' # Simple usage
#' add_atc_classification(df)
#'
#' # Different level
#' add_atc_classification(df, atc_level = "third")
#' }
add_atc_classification <- function(dataf,
                                   rxcui_col = "rxcui",
                                   new_col_name = NULL,
                                   unnest = FALSE,
                                   atc_level = "second",
                                   batch_size = 500,
                                   save_freq = 500,
                                   cache_dir = "cache",
                                   max_age_days = 30,
                                   retry_count = 3,
                                   batch_delay = 0.5) {

  start_time <- Sys.time()

  # Auto-gen column name based on ATC level if not provided
  if (is.null(new_col_name)) {
    level_num <- switch(atc_level,
                        "first" = "1",
                        "second" = "2",
                        "third" = "3",
                        "fourth" = "4",
                        "2")  # fallback
    new_col_name <- paste0("atc", level_num, "_class")
  }

  .validate_atc_inputs(dataf, rxcui_col, new_col_name, unnest, atc_level)

  rxcui_info <- .get_unique_rxcuis(dataf, rxcui_col)

  if (rxcui_info$count == 0) {
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # Create level-specific cache name
  level_num <- switch(atc_level,
                      "first" = "1",
                      "second" = "2",
                      "third" = "3",
                      "fourth" = "4",
                      "2")

  cache_name <- paste0("atc", level_num, "_cache")

  # Process batch with anonymous function that includes atc_level
  rxcui_to_atc2 <- .process_batch(
    items = rxcui_info$unique_rxcuis,
    api_function = function(rxcui) rxnorm::get_atc(rxcui, query_atc = atc_level),
    batch_size = batch_size,
    save_freq = save_freq,
    cache_name = cache_name,
    cache_dir = cache_dir,
    max_age_days = max_age_days,
    retry_count = retry_count,
    batch_delay = batch_delay,
    process_type = paste0("ATC", level_num, " classifications")
  )

  if (unnest) {
    result_df <- .apply_atc2_unnested(dataf, rxcui_col, new_col_name, rxcui_to_atc2)
  } else {
    result_df <- .apply_atc2_nested(dataf, rxcui_col, new_col_name, rxcui_to_atc2)
  }

  .report_atc2_results(result_df, new_col_name, start_time)

  return(result_df)
}

#' @keywords internal
#' @noRd
.validate_atc_inputs <- function(dataf, rxcui_col, new_col_name, unnest, atc_level) {
  if (!is.data.frame(dataf) || nrow(dataf) == 0) {
    cli::cli_abort("Input must be a non-empty data frame")
  }
  if (!rxcui_col %in% names(dataf)) {
    cli::cli_abort("Column '{rxcui_col}' not found")
  }
  if (!is.logical(unnest)) {
    cli::cli_abort("{.arg unnest} must be TRUE or FALSE")
  }
  if (new_col_name %in% names(dataf)) {
    cli::cli_alert_warning("Column '{new_col_name}' will be overwritten")
  }
  valid_levels <- c("first", "second", "third", "fourth")
  if (!atc_level %in% valid_levels) {
    cli::cli_abort("{.arg atc_level} must be one of: {.val {valid_levels}}")
  }
}


#' @keywords internal
#' @noRd
.get_unique_rxcuis <- function(dataf, rxcui_col) {
  rxcuis <- dataf[[rxcui_col]]
  valid <- rxcuis[!is.na(rxcuis)]

  list(unique_rxcuis = unique(as.character(valid)), count = length(unique(valid)))
}


#' @keywords internal
#' @noRd
.apply_atc2_nested <- function(dataf, rxcui_col, new_col_name, rxcui_to_atc2) {
  dataf[[new_col_name]] <- vapply(dataf[[rxcui_col]], function(rxcui) {
    if (is.na(rxcui)) return(NA_character_)
    atc2 <- rxcui_to_atc2[[as.character(rxcui)]]
    if (is.null(atc2) || all(is.na(atc2))) return(NA_character_)
    if (length(atc2) > 1) paste(atc2, collapse = "; ") else as.character(atc2)
  }, character(1))

  dataf
}


#' @keywords internal
#' @noRd
.apply_atc2_unnested <- function(dataf, rxcui_col, new_col_name, rxcui_to_atc2) {
  atc2_mapping <- do.call(rbind, lapply(names(rxcui_to_atc2), function(rxcui) {
    atc2 <- rxcui_to_atc2[[rxcui]]
    if (is.null(atc2) || all(is.na(atc2))) {
      data.frame(rxcui = rxcui, atc2_class = NA_character_, stringsAsFactors = FALSE)
    } else {
      data.frame(rxcui = rxcui, atc2_class = atc2, stringsAsFactors = FALSE)
    }
  }))

  if (new_col_name != "atc2_class") names(atc2_mapping)[2] <- new_col_name

  dplyr::left_join(dataf, atc2_mapping,
                   by = stats::setNames("rxcui", rxcui_col),
                   relationship = "many-to-many")
}


#' @keywords internal
#' @noRd
.report_atc2_results <- function(result_df, new_col_name, start_time) {
  found <- sum(!is.na(result_df[[new_col_name]]))
  total <- nrow(result_df)
  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)

  cli::cli_alert_success("ATC2: {found}/{total} ({round(100*found/total,1)}%) in {elapsed} min")
}
