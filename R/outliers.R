#' Detect Outlier Thresholds
#'
#' Calculates lower and upper bounds for outlier detection using the
#' IQR method (Q1 - k*IQR and Q3 + k*IQR).
#'
#' @param dataf A data frame containing the numeric variable
#' @param var_name Character. Name of the numeric variable to analyze
#' @param multiplier Numeric. Multiplier for IQR (default: 1.5)
#' @param label Character. Optional custom label for output (default: NULL, uses var_name)
#'
#' @return Data frame with lower bound (LB), upper bound (UB), and notes
#' @export
#'
#' @examples
#' \dontrun{
#' thresholds <- detect_outlier_thresholds(df, "age")
#' thresholds <- detect_outlier_thresholds(df, "weight", multiplier = 2)
#' }
detect_outlier_thresholds <- function(dataf, var_name, multiplier = 1.5, label = NULL) {

  adRutils::validate_params(
    data = dataf,
    columns = var_name,
    numeric_columns = var_name,
    custom_checks = list(
      list(
        condition = is.numeric(multiplier) && multiplier > 0,
        message = "{.arg multiplier} must be a positive number"
      )
    ),
    context = "detect_outlier_thresholds"
  )

  # Calculate quartiles and bounds
  q1 <- stats::quantile(dataf[[var_name]], 0.25, na.rm = TRUE)
  q3 <- stats::quantile(dataf[[var_name]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr

  # Return results
  data.frame(
    LB = lower_bound,
    UB = upper_bound,
    notes = if (is.null(label)) var_name else label,
    stringsAsFactors = FALSE
  )
}

#' Remove Outliers from Numeric Variables
#'
#' Identifies outliers in numeric variables and replaces them with NA.
#' This function is idempotent - running it multiple times with the same
#' parameters produces the same result.
#'
#' @param dataf A data frame
#' @param var_names Character vector. Names of numeric variables to check for outliers
#' @param multiplier Numeric. Multiplier for IQR in outlier detection (default: 1.5)
#' @param paired_cols Named list. Optional pairing of transformed columns with original columns
#' @param remove_all_na_rows Logical. If TRUE, removes rows where all specified variables are NA (default: TRUE)
#' @param verbose Logical. If TRUE, shows informative messages (default: TRUE)
#'
#' @return Data frame with outliers replaced by NA
#' @export
replace_outliers_with_na <- function(dataf, var_names, multiplier = 1.5,
                                     paired_cols = NULL, remove_all_na_rows = TRUE,
                                     verbose = TRUE) {

  adRutils::validate_params(
    data = dataf,
    columns = var_names,
    custom_checks = list(
      list(
        condition = is.numeric(multiplier) && multiplier > 0,
        message = "{.arg multiplier} must be a positive number"
      ),
      list(
        condition = is.logical(remove_all_na_rows) && length(remove_all_na_rows) == 1,
        message = "{.arg remove_all_na_rows} must be TRUE or FALSE"
      ),
      list(
        condition = is.logical(verbose) && length(verbose) == 1,
        message = "{.arg verbose} must be TRUE or FALSE"
      ),
      list(
        condition = is.null(paired_cols) || is.list(paired_cols),
        message = "{.arg paired_cols} must be NULL or a named list"
      )
    ),
    context = "replace_outliers_with_na"
  )

  result_df <- dataf
  total_outliers <- 0

  # Process each variable
  for (var in var_names) {
    if (!is.numeric(result_df[[var]])) {
      if (verbose) cli::cli_alert_warning("Skipping non-numeric column: {var}")
      next
    }

    # Replace outliers for this variable
    outlier_result <- .replace_outliers_single_var(
      result_df, var, multiplier, paired_cols
    )
    result_df <- outlier_result$data
    total_outliers <- total_outliers + outlier_result$n_outliers
  }

  # Remove all-NA rows if requested
  if (remove_all_na_rows) {
    result_df <- .remove_all_na_rows(result_df, dataf, var_names, verbose)
  }

  # Final summary message
  if (verbose) {
    if (total_outliers > 0) {
      cli::cli_alert_success("Replaced {total_outliers} outlier{?s} with NA across {length(var_names)} variable{?s}")
    } else {
      cli::cli_alert_info("No outliers detected")
    }
  }

  return(result_df)
}


#' Replace outliers for a single variable
#' @keywords internal
.replace_outliers_single_var <- function(dataf, var, multiplier, paired_cols) {
  # Get outlier bounds
  cutoffs <- detect_outlier_thresholds(dataf, var, multiplier)

  # Identify outliers
  outliers <- !is.na(dataf[[var]]) &
    (dataf[[var]] < cutoffs$LB | dataf[[var]] > cutoffs$UB)

  n_outliers <- sum(outliers)

  # Replace outliers in main variable
  if (n_outliers > 0) {
    dataf[outliers, var] <- NA
  }

  # Handle paired columns
  if (!is.null(paired_cols) && var %in% names(paired_cols)) {
    paired_var <- paired_cols[[var]]
    if (paired_var %in% names(dataf)) {
      dataf[outliers, paired_var] <- NA
    }
  }

  list(data = dataf, n_outliers = n_outliers)
}

#' Remove rows where all specified variables are NA
#' @keywords internal
.remove_all_na_rows <- function(result_df, original_df, var_names, verbose) {
  all_na_rows <- apply(result_df[var_names], 1, function(row) all(is.na(row)))
  n_removed <- sum(all_na_rows)

  if (n_removed > 0) {
    result_df <- result_df[!all_na_rows, ]
    if (verbose) {
      cli::cli_alert_info("Removed {n_removed} rows with all NAs ({round(100 * n_removed / nrow(original_df), 1)}%)")
    }
  }

  return(result_df)
}
