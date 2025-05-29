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
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  if (!var_name %in% colnames(dataf)) {
    stop("The data must contain column: ", var_name)
  }

  if (!is.numeric(dataf[[var_name]])) {
    stop("Column '", var_name, "' must be numeric.")
  }

  # Calculate quartiles and IQR
  q3 <- stats::quantile(dataf[[var_name]], 0.75, na.rm = TRUE)
  q1 <- stats::quantile(dataf[[var_name]], 0.25, na.rm = TRUE)
  iqr <- stats::IQR(dataf[[var_name]], na.rm = TRUE)

  # Calculate outlier boundaries
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr

  # Use provided label or create one from column name
  if (is.null(label)) {
    # Default label extraction logic
    custom_label <- stringr::str_split(var_name, "_")[[1]]
    if (length(custom_label) >= 4) {
      parts_to_keep <- custom_label[-c(3, 4)]
    } else {
      parts_to_keep <- custom_label
    }
    final_label <- paste(parts_to_keep, collapse = "_")
  } else {
    final_label <- label
  }

  # Create result data frame
  out_lb_ub <- data.frame(
    LB = lower_bound,
    UB = upper_bound,
    notes = final_label
  )

  return(out_lb_ub)
}

#' Remove Outliers from Numeric Variables
#'
#' Identifies outliers in numeric variables and replaces them with NA.
#' Tracks processing to prevent duplicate outlier removal.
#'
#' @param dataf A data frame
#' @param var_names Character vector. Names of numeric variables to check for outliers
#' @param multiplier Numeric. Multiplier for IQR in outlier detection (default: 1.5)
#' @param paired_cols Named list. Optional pairing of transformed columns with original
#'                  columns (e.g., list("log10_var" = "var"))
#' @param remove_all_na_rows Logical. If TRUE, removes rows where all specified variables are NA (default: TRUE)
#' @param force Logical. If TRUE, bypasses the check for previous processing (default: FALSE)
#'
#' @return Data frame with outliers replaced by NA
#' @export

replace_outliers_with_na <- function(dataf, var_names, multiplier = 1.5,
                                     paired_cols = NULL, remove_all_na_rows = TRUE,
                                     force = FALSE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  missing_cols <- var_names[!var_names %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if paired columns exist
  if (!is.null(paired_cols)) {
    missing_paired <- names(paired_cols)[!paired_cols %in% names(dataf)]
    if (length(missing_paired) > 0) {
      stop("The following paired columns do not exist in the data: ",
           paste(missing_paired, collapse = ", "))
    }
  }

  # Check if these columns have already been processed
  if (!force) {
    adRutils::is_processed("replace_outliers_with_na", var_names, error_if_exists = TRUE)
  }

  # Create a copy of the data frame to modify
  result_df <- dataf

  # Process each variable
  for (var in var_names) {
    # Skip non-numeric columns with warning
    if (!is.numeric(result_df[[var]])) {
      warning("Skipping non-numeric column: ", var)
      next
    }

    # Get outlier cutoffs
    cutoffs_df <- detect_outlier_thresholds(result_df, var, multiplier)
    lower_bound <- cutoffs_df[["LB"]]
    upper_bound <- cutoffs_df[["UB"]]

    # Find outliers using logical indexing (with NA handling)
    below_cutoff <- !is.na(result_df[[var]]) & result_df[[var]] <= lower_bound
    above_cutoff <- !is.na(result_df[[var]]) & result_df[[var]] >= upper_bound

    # Replace outliers with NA in the variable
    result_df[below_cutoff, var] <- NA
    result_df[above_cutoff, var] <- NA

    # Also replace in paired column if specified
    if (!is.null(paired_cols) && var %in% names(paired_cols)) {
      paired_var <- paired_cols[[var]]
      if (paired_var %in% names(result_df)) {
        result_df[below_cutoff, paired_var] <- NA
        result_df[above_cutoff, paired_var] <- NA
      }
    }

    # Report on outliers
    n_outliers <- sum(below_cutoff) + sum(above_cutoff)
    if (n_outliers > 0) {
      message(sprintf("Removed %d outliers from %s (%.1f%%)",
                      n_outliers, var, 100 * n_outliers / nrow(result_df)))
    }
  }

  # Remove rows with all NA values in the specified variables if requested
  if (remove_all_na_rows) {
    rows_to_keep <- !apply(result_df[var_names], 1, function(row) all(is.na(row)))
    n_removed <- sum(!rows_to_keep)

    if (n_removed > 0) {
      message(sprintf("Removing %d rows with all NAs in specified variables (%.1f%%)",
                      n_removed, 100 * n_removed / nrow(result_df)))
      result_df <- result_df[rows_to_keep, ]
    }
  }

  # Register these variables as processed
  adRutils::register_processed("replace_outliers_with_na", var_names)

  return(result_df)
}
