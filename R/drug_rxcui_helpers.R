#' Validate inputs for add_rxcuis function
#' @noRd
.validate_rxcui_inputs <- function(dataf, med_column, rxcui_column, method,
                                   batch_size, save_freq, cache_dir, max_age_days,
                                   retry_count, batch_delay) {

  adRutils::validate_params(
    data = dataf,
    columns = med_column,
    custom_checks = list(
      # rxcui_column validation
      list(
        condition = is.character(rxcui_column) && length(rxcui_column) == 1 &&
          !is.na(rxcui_column) && rxcui_column != "",
        message = "rxcui_column must be a single non-empty character string"
      ),
      # method validation
      list(
        condition = method %in% c("exact", "approximate"),
        message = "method must be either 'exact' or 'approximate'"
      ),
      # batch_size validation
      list(
        condition = is.numeric(batch_size) && length(batch_size) == 1 &&
          batch_size > 0 && batch_size <= 1000,
        message = "batch_size must be a single number between 1 and 1000"
      ),
      # save_freq validation
      list(
        condition = is.numeric(save_freq) && length(save_freq) == 1 && save_freq > 0,
        message = "save_freq must be a single positive number"
      ),
      # cache_dir validation
      list(
        condition = is.character(cache_dir) && length(cache_dir) == 1 && cache_dir != "",
        message = "cache_dir must be a single non-empty character string"
      ),
      # max_age_days validation
      list(
        condition = is.numeric(max_age_days) && length(max_age_days) == 1 && max_age_days > 0,
        message = "max_age_days must be a single positive number"
      ),
      # retry_count validation
      list(
        condition = is.numeric(retry_count) && length(retry_count) == 1 &&
          retry_count >= 1 && retry_count <= 10,
        message = "retry_count must be a single number between 1 and 10"
      ),
      # batch_delay validation
      list(
        condition = is.numeric(batch_delay) && length(batch_delay) == 1 && batch_delay >= 0,
        message = "batch_delay must be a single non-negative number"
      ),
      # Data frame has rows
      list(
        condition = nrow(dataf) > 0,
        message = "Input data frame is empty (0 rows)"
      )
    ),
    context = "add_rxcuis"
  )
}

#' Setup result data frame with RxCUI column
#' @noRd
.setup_result_dataframe <- function(dataf, rxcui_column) {
  result_df <- dataf

  # Add RxCUI column if it doesn't exist
  if (!rxcui_column %in% names(result_df)) {
    result_df[[rxcui_column]] <- NA_character_
  } else {
    # Warn if column exists and will be overwritten
    cli::cli_alert_warning("Column '{rxcui_column}' already exists and will be overwritten.")
  }

  return(result_df)
}

#' Get unique medications from data frame
#' @noRd
.get_unique_medications <- function(dataf, med_column) {
  # Get all medication values
  meds <- dataf[[med_column]]

  # Filter to non-NA, non-empty values
  valid_meds <- meds[!is.na(meds) & meds != ""]
  unique_meds <- unique(valid_meds)

  cli::cli_alert_info("Found {length(unique_meds)} unique medications out of { length(valid_meds)} total valid entries ")

  return(list(
    unique_meds = unique_meds,
    count = length(unique_meds),
    total_valid = length(valid_meds)
  ))
}

#' Report final processing results
#' @noRd
.report_results <- function(result_df, original_df, rxcui_column, med_column, start_time, method) {
  found_count <- sum(!is.na(result_df[[rxcui_column]]))
  valid_count <- sum(!is.na(original_df[[med_column]]) & original_df[[med_column]] != "")

  cli_h2(col_cyan("RxCUI RESULTS"))
  cli_ul()
  cli_li("Method: {method}")
  cli_li("Coverage: {found_count}/{valid_count} ({round(found_count/valid_count*100, 1)}%)")

  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cli_li("Time: {round(elapsed, 1)} minutes ")
  cli_end()
  cli_rule()
}

#' Null-coalescing operator
#' @noRd
.null_coalesce <- function(x, y) {
  if (is.null(x) || is.na(x)) y else x
}
