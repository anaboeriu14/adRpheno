#' Add Medication Categories Based on ATC Codes and Medication Names
#'
#' Classifies medications into predefined categories (cholesterol, diabetes,
#' hypertension, and GLP-1) based on ATC codes and medication names using
#' regular expression pattern matching.
#'
#' @param dataf A data frame containing medication data
#' @param med_col Character. Column name containing medication names
#' @param atc_col Character. Column name containing ATC codes
#' @param cholesterol_patterns Character vector. Patterns for cholesterol medications
#' @param diabetes_patterns Character vector. Patterns for diabetes medications
#' @param hypertension_patterns Character vector. Patterns for hypertension medications
#' @param glp1_patterns Character vector. Patterns for GLP-1 medications
#' @param output_prefix Character. Prefix for output category columns (default: "")
#' @param check_both Logical. Check both medication names and ATC codes for all categories (default: FALSE)
#' @param force_reprocess Logical. Force reprocessing (default: FALSE)
#'
#' @return Data frame with additional binary columns (1=yes, 0=no) for each medication category
#' @export
categorize_medications <- function(dataf,
                                    med_col,
                                    atc_col,
                                    cholesterol_patterns,
                                    diabetes_patterns,
                                    hypertension_patterns,
                                    glp1_patterns,
                                    output_prefix = "",
                                    check_both = FALSE,
                                    force_reprocess = FALSE) {

  # Validate input
  if (!is.data.frame(dataf)) stop("'dataf' must be a data frame")
  if (!med_col %in% names(dataf)) stop(paste("Medication column", med_col, "not found in dataframe"))
  if (!atc_col %in% names(dataf)) stop(paste("ATC column", atc_col, "not found in dataframe"))

  # Validate pattern inputs
  pattern_args <- list(cholesterol_patterns, diabetes_patterns, hypertension_patterns, glp1_patterns)
  if (!all(sapply(pattern_args, is.character))) {
    stop("All pattern arguments must be character vectors")
  }

  # Check that dplyr is available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for this function")
  }

  # Check if already processed
  if (!force_reprocess) {
    # Generate a list of expected output columns
    category_suffixes <- c("cholesterol_med", "diabetes_med", "hypertension_med", "glp1_med")
    expected_cols <- paste0(output_prefix, category_suffixes)

    # Check if all expected columns already exist
    if (all(expected_cols %in% names(dataf))) {
      message("Medication categories already added to dataframe. Use force_reprocess=TRUE to reprocess.")
      return(dataf)
    }
  }

  # Create regex patterns with word boundaries
  make_regex <- function(patterns) {
    paste0("\\b(", paste(patterns, collapse = "|"), ")\\b")
  }

  cholesterol_regex <- make_regex(cholesterol_patterns)
  diabetes_regex <- make_regex(diabetes_patterns)
  hypertension_regex <- make_regex(hypertension_patterns)
  glp1_regex <- make_regex(glp1_patterns)

  # Generate column names
  cholesterol_col <- paste0(output_prefix, "cholesterol_med")
  diabetes_col <- paste0(output_prefix, "diabetes_med")
  hypertension_col <- paste0(output_prefix, "hypertension_med")
  glp1_col <- paste0(output_prefix, "glp1_med")

  # Get column symbols for tidy evaluation
  atc_col_sym <- rlang::ensym(atc_col)
  med_col_sym <- rlang::ensym(med_col)

  # Define a helper function to detect patterns
  detect_pattern <- function(col, pattern) {
    dplyr::if_else(!is.na(col) & grepl(pattern, col, ignore.case = TRUE), 1, 0)
  }

  # Use dplyr::mutate to add the category columns
  if (check_both) {
    # Check both ATC codes and medication names for all categories
    result_df <- dataf %>%
      dplyr::mutate(
        !!cholesterol_col := pmax(
          detect_pattern(!!atc_col_sym, cholesterol_regex),
          detect_pattern(!!med_col_sym, cholesterol_regex)
        ),
        !!diabetes_col := pmax(
          detect_pattern(!!atc_col_sym, diabetes_regex),
          detect_pattern(!!med_col_sym, diabetes_regex)
        ),
        !!hypertension_col := pmax(
          detect_pattern(!!atc_col_sym, hypertension_regex),
          detect_pattern(!!med_col_sym, hypertension_regex)
        ),
        !!glp1_col := pmax(
          detect_pattern(!!atc_col_sym, glp1_regex),
          detect_pattern(!!med_col_sym, glp1_regex)
        )
      )
  } else {
    # Check ATC codes for all categories, but medication names only for GLP-1
    result_df <- dataf %>%
      dplyr::mutate(
        !!cholesterol_col := detect_pattern(!!atc_col_sym, cholesterol_regex),
        !!diabetes_col := detect_pattern(!!atc_col_sym, diabetes_regex),
        !!hypertension_col := detect_pattern(!!atc_col_sym, hypertension_regex),
        !!glp1_col := pmax(
          detect_pattern(!!atc_col_sym, glp1_regex),
          detect_pattern(!!med_col_sym, glp1_regex)
        )
      )
  }

  # Report results
  chol_count <- sum(result_df[[cholesterol_col]], na.rm = TRUE)
  diab_count <- sum(result_df[[diabetes_col]], na.rm = TRUE)
  hyp_count <- sum(result_df[[hypertension_col]], na.rm = TRUE)
  glp1_count <- sum(result_df[[glp1_col]], na.rm = TRUE)

  message("Added medication categories:")
  message("  - ", chol_count, " cholesterol medications")
  message("  - ", diab_count, " diabetes medications")
  message("  - ", hyp_count, " hypertension medications")
  message("  - ", glp1_count, " GLP-1 medications")

  return(result_df)
}
