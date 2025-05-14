#'  Biomarker Outlier Functions
#'
#'  Functions for identifying and handling outliers in biomarker data.
#' Calculates outlier bounds for biomarkers
#'
#' Calculates lower and upper bounds for outlier detection using the
#' IQR method (Q1 - 1.5*IQR and Q3 + 1.5*IQR).
#'
#' @param dataf A data frame
#' @param log_10_biom_column Character. Name of the log10-transformed biomarker column
#'
#' @return Data frame with lower bound (LB), upper bound (UB), and notes
#' @export
calculate_outlier_bounds <- function(dataf, log_10_biom_column) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  if (!log_10_biom_column %in% colnames(dataf)) {
    stop("The data must contain column: ", log_10_biom_column)
  }

  # Calculate quartiles and IQR
  q3 <- stats::quantile(dataf[[log_10_biom_column]], 0.75, na.rm = TRUE)
  q1 <- stats::quantile(dataf[[log_10_biom_column]], 0.25, na.rm = TRUE)
  iqr <- stats::IQR(dataf[[log_10_biom_column]], na.rm = TRUE)

  # Calculate outlier boundaries
  small_outlier <- q1 - 1.5 * iqr
  large_outlier <- q3 + 1.5 * iqr

  # Create label from column name
  custom_label <- stringr::str_split(log_10_biom_column, "_")[[1]]
  # Adjust this if your column naming convention is different
  if (length(custom_label) >= 4) {
    parts_to_keep <- custom_label[-c(3, 4)]
  } else {
    parts_to_keep <- custom_label
  }
  final_label <- paste(parts_to_keep, collapse = "_")

  # Create result data frame
  out_lb_ub <- data.frame(
    LB = small_outlier,
    UB = large_outlier,
    notes = final_label
  )

  return(out_lb_ub)
}

#' Remove biomarker outliers
#'
#' Identifies outliers in biomarker values and replaces them with NA.
#' Tracks processing to prevent duplicate outlier removal.
#'
#' @param dataf A data frame
#' @param log_biom_cols Character vector. Names of biomarker columns to check for outliers
#' @param force Logical. If TRUE, bypasses the check for previous processing. Default is FALSE.
#' @param remove_all_na_rows Logical. If TRUE, removes rows where all specified biomarkers are NA.
#'
#' @return Data frame with outliers replaced by NA
#' @export
remove_biomarker_outliers <- function(dataf, log_biom_cols, force = FALSE, remove_all_na_rows = TRUE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  missing_cols <- log_biom_cols[!log_biom_cols %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if these columns have already been processed
  if (!force) {
    adRutils::is_processed("remove_biomarker_outliers", log_biom_cols, error_if_exists = TRUE)
  }

  # Create a copy of the data frame to modify
  result_df <- dataf

  # Process each biomarker column
  for (i in log_biom_cols) {
    # Get outlier cutoffs
    cutoffs_df <- calculate_outlier_bounds(result_df, i)
    lower_bound <- cutoffs_df[["LB"]]
    upper_bound <- cutoffs_df[["UB"]]

    # Determine original column name (without log10_ prefix)
    orig_biom_col <- stringr::str_replace(i, "log10_", "")

    # Find outliers using logical indexing (with NA handling)
    below_cutoff <- !is.na(result_df[[i]]) & result_df[[i]] <= lower_bound
    above_cutoff <- !is.na(result_df[[i]]) & result_df[[i]] >= upper_bound

    # Replace outliers with NA in BOTH the log10 column AND the original column
    result_df[below_cutoff, i] <- NA  # Set to NA in log10 column
    result_df[above_cutoff, i] <- NA  # Set to NA in log10 column

    # Make sure the original column exists before trying to modify it
    if (orig_biom_col %in% names(result_df)) {
      result_df[below_cutoff, orig_biom_col] <- NA  # Set to NA in original column
      result_df[above_cutoff, orig_biom_col] <- NA  # Set to NA in original column
    }

    # Report on outliers
    n_outliers <- sum(below_cutoff) + sum(above_cutoff)
    if (n_outliers > 0) {
      message(sprintf("Removed %d outliers from %s (%.1f%%)",
                      n_outliers, i, 100 * n_outliers / nrow(result_df)))
    }
  }

  # Remove rows with all NA values in the specified biomarker columns if requested
  if (remove_all_na_rows) {
    rows_to_keep <- !apply(result_df[log_biom_cols], 1, function(row) all(is.na(row)))
    n_removed <- sum(!rows_to_keep)

    if (n_removed > 0) {
      message(sprintf("Removing %d rows with all NAs in specified biomarkers (%.1f%%)",
                      n_removed, 100 * n_removed / nrow(result_df)))
      result_df <- result_df[rows_to_keep, ]
    }
  }

  # Register these biomarker columns as processed
  adRutils::register_processed("remove_biomarker_outliers", log_biom_cols)

  return(result_df)
}
