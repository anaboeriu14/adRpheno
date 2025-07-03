#' Add ATC2 therapeutic classifications to medications
#'
#' This function takes a data frame with RxCUIs and adds ATC2 therapeutic
#' classifications. ATC codes classify drugs by their therapeutic use.
#'
#' @param dataf Data frame containing RxCUI values
#' @param rxcui_col Character. Column name containing RxCUI values (default: "rxcui")
#' @param new_col_name Character. Name for the new ATC2 column (default: "atc2_class")
#' @param unnest Logical. If TRUE, creates separate rows for each ATC2 class;
#'               if FALSE, combines multiple classes with semicolons (default: FALSE)
#' @param batch_size Integer. Number of RxCUIs per batch (default: 200)
#' @param save_freq Integer. Save cache every N items (default: 50)
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param max_age_days Integer. Cache expiration in days (default: 30)
#' @param retry_count Integer. Number of retry attempts (default: 3)
#' @param batch_delay Numeric. Seconds between batches (default: 2)
#'
#' @return Data frame with ATC2 classifications added
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage (nested - multiple classes in one cell)
#' df_with_atc2 <- add_atc2_classification(df_with_rxcuis)
#'
#' # Unnested (separate row for each ATC2 class)
#' df_unnested <- add_atc2_classification(df_with_rxcuis, unnest = TRUE)
#'
#' # Custom column names
#' df_custom <- add_atc2_classification(df, rxcui_col = "my_rxcui",
#'                                     new_col_name = "therapeutic_class")
#' }
add_atc2_classification <- function(dataf,
                                    rxcui_col = "rxcui",
                                    new_col_name = "atc2_class",
                                    unnest = FALSE,
                                    batch_size = 200,
                                    save_freq = 50,
                                    cache_dir = "cache",
                                    max_age_days = 30,
                                    retry_count = 3,
                                    batch_delay = 2) {

  # === STEP 1: Validate inputs ===
  .validate_atc2_inputs(dataf, rxcui_col, new_col_name, unnest,
                        batch_size, save_freq, cache_dir, max_age_days,
                        retry_count, batch_delay)

  # === STEP 2: Check for existing column ===
  if (new_col_name %in% names(dataf)) {
    warning("Column '", new_col_name, "' already exists and will be overwritten.",
            call. = FALSE)
  }

  # === STEP 3: Get unique RxCUIs to process ===
  rxcui_info <- .get_unique_rxcuis(dataf, rxcui_col)

  if (rxcui_info$count == 0) {
    warning("No valid RxCUIs found in the data frame")
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # === STEP 4: Process RxCUIs using batch processor with CLI progress ===
  rxcui_to_atc2 <- .process_batch(
    items = rxcui_info$unique_rxcuis,
    api_function = rxnorm::get_atc,
    batch_size = batch_size,
    save_freq = save_freq,
    cache_name = "atc2_cache",
    cache_dir = cache_dir,
    max_age_days = max_age_days,
    retry_count = retry_count,
    batch_delay = batch_delay,
    process_type = "ATC2 classifications",
    "second"  # ATC level passed to rxnorm::get_atc
  )

  # === STEP 5: Apply results based on nesting option ===
  if (unnest) {
    result_df <- .apply_atc2_unnested(dataf, rxcui_col, new_col_name, rxcui_to_atc2)
    message("Created unnested data frame with ", nrow(result_df) - nrow(dataf),
            " additional rows for multiple ATC2 classifications")
  } else {
    result_df <- .apply_atc2_nested(dataf, rxcui_col, new_col_name, rxcui_to_atc2)
    multi_class_count <- sum(grepl(";", result_df[[new_col_name]]), na.rm = TRUE)
    if (multi_class_count > 0) {
      message(multi_class_count, " medications have multiple ATC2 classifications (combined with ';')")
    }
  }

  # === STEP 6: Report results ===
  .report_atc2_results(result_df, new_col_name)

  return(result_df)
}
