#' Detect Outlier Thresholds
#'
#' Calculates lower and upper bounds for outlier detection using the
#' IQR method (Q1 - k*IQR and Q3 + k*IQR).
#'
#' @param dataf A data frame containing the numeric variable
#' @param var_name Character. Name of the numeric variable to analyze
#' @param multiplier Numeric. Multiplier for IQR (default: 1.5)
#' @param label Character. Optional custom label for the output (default: NULL, uses var_name)
#'
#' @return Data frame with lower bound (LB), upper bound (UB), and notes
#' @export
detect_outlier_thresholds <- function(dataf, var_name, multiplier = 1.5, label = NULL) {

  # === STEP 1: Simple validation ===
  adRutils::validate_params(
    data = dataf,
    columns = var_name,
    numeric_columns = var_name,
    custom_checks = list(
      list(
        condition = is.numeric(multiplier) && multiplier > 0,
        message = "multiplier must be a positive number"
      )
    ),
    context = "detect_outlier_thresholds"
  )
  # === STEP 2: Calculate quartiles and bounds ===
  q1 <- stats::quantile(dataf[[var_name]], 0.25, na.rm = TRUE)
  q3 <- stats::quantile(dataf[[var_name]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr

  # === STEP 3: Return data frame exactly as you want ===
  out_lb_ub = data.frame(
    LB = lower_bound,
    UB = upper_bound,
    notes = if (is.null(label)) var_name else label
  )
  return(out_lb_ub)
}

#' Remove Outliers from Numeric Variables
#'
#' Identifies outliers in numeric variables and replaces them with NA.
#'
#' @param dataf A data frame
#' @param var_names Character vector. Names of numeric variables to check for outliers
#' @param multiplier Numeric. Multiplier for IQR in outlier detection (default: 1.5)
#' @param paired_cols Named list. Optional pairing of transformed columns with original columns
#' @param remove_all_na_rows Logical. If TRUE, removes rows where all specified variables are NA (default: TRUE)
#' @param force Logical. If TRUE, bypasses the check for previous processing (default: FALSE)
#'
#' @return Data frame with outliers replaced by NA
#' @export
replace_outliers_with_na <- function(dataf, var_names, multiplier = 1.5,
                                     paired_cols = NULL, remove_all_na_rows = TRUE,
                                     force = FALSE) {

  # === STEP 1: Simple validation ===
  adRutils::validate_params(
    data = dataf,
    columns = var_names,
    custom_checks = list(
      list(
        condition = is.numeric(multiplier) && multiplier > 0,
        message = "multiplier must be a positive number"
      ),
      list(
        condition = is.logical(remove_all_na_rows) && length(remove_all_na_rows) == 1,
        message = "remove_all_na_rows must be TRUE or FALSE"
      ),
      list(
        condition = is.logical(force) && length(force) == 1,
        message = "force must be TRUE or FALSE"
      ),
      list(
        condition = is.null(paired_cols) || is.list(paired_cols),
        message = "paired_cols must be NULL or a named list"
      )
    ),
    context = "replace_outliers_with_na"
  )

  # === STEP 2: Check processing history ===
  if (!force) {
    adRutils::is_processed("replace_outliers_with_na", var_names, error_if_exists = TRUE)
  }

  # === STEP 3: Process each variable ===
  result_df <- dataf
  processed_vars <- character()
  total_outliers <- 0  # Track total outliers removed

  for (var in var_names) {
    # Skip non-numeric columns
    if (!is.numeric(result_df[[var]])) {
      warning("Skipping non-numeric column: ", var, call. = FALSE)
      next
    }

    # Get outlier bounds
    cutoffs <- detect_outlier_thresholds(result_df, var, multiplier)

    # Find and replace outliers
    outliers <- !is.na(result_df[[var]]) &
      (result_df[[var]] <= cutoffs$LB | result_df[[var]] >= cutoffs$UB)

    result_df[outliers, var] <- NA

    # Handle paired columns
    if (!is.null(paired_cols) && var %in% names(paired_cols)) {
      paired_var <- paired_cols[[var]]
      if (paired_var %in% names(result_df)) {
        result_df[outliers, paired_var] <- NA
      }
    }

    # Track progress
    n_outliers <- sum(outliers)
    total_outliers <- total_outliers + n_outliers
    processed_vars <- c(processed_vars, var)  # FIXED: Add variable to processed list
  }

  # === STEP 4: Remove all-NA rows if requested ===
  if (remove_all_na_rows && length(processed_vars) > 0) {
    all_na_rows <- apply(result_df[processed_vars], 1, function(row) all(is.na(row)))
    n_removed <- sum(all_na_rows)

    if (n_removed > 0) {
      message(sprintf("Removing %d rows with all NAs (%.1f%%)",
                      n_removed, 100 * n_removed / nrow(result_df)))
      result_df <- result_df[!all_na_rows, ]
    }
  }

  # === STEP 5: Complete processing ===
  if (length(processed_vars) > 0) {
    adRutils::register_processed("replace_outliers_with_na", processed_vars)

    # Show summary instead of individual messages
    if (total_outliers > 0) {
      message("Removed ", total_outliers, " outliers across ", length(processed_vars), " variables")
    }
    message("Outlier removal complete (", length(processed_vars), " variables processed)")
  }

  return(result_df)
}


