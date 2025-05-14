#' @title Biomarker Standardization Functions
#' @description Functions for standardizing biomarker values and combining data across batches

#' Standardize biomarker values
#'
#' Creates standardized (z-score) versions of biomarker values.
#' Tracks processing to prevent duplicate standardization.
#'
#' @param dataf A data frame
#' @param bioms Character vector. Names of biomarker columns to standardize
#' @param force Logical. If TRUE, bypasses the check for previous processing. Default is FALSE.
#'
#' @return Data frame with added standardized biomarker columns (prefix "zscore_")
#' @export
#'
#' @examples
#' # Standardize biomarker columns
#' standardize_biomarkers(biomarker_data, c("protein_a", "protein_b"))
standardize_biomarkers <- function(dataf, bioms, force = FALSE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  missing_cols <- bioms[!bioms %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if these columns have already been processed
  if (!force) {
    adRutils::is_processed("standardize_biomarkers", bioms, error_if_exists = TRUE)
  }

  # Create z-scores
  df_standardized_bioms <- dataf %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(bioms), ~ as.vector(scale(.)),
                                .names = "zscore_{stringr::str_remove(.col, 'qtx_plasma_')}"))

  # Register these columns as processed
  adRutils::register_processed("standardize_biomarkers", bioms)

  return(df_standardized_bioms)
}

#' Combine batch biomarker data
#'
#' Combines biomarker data from multiple batches (e.g., r3, r5),
#' prioritizing earlier batches when values are available.
#'
#' @param dataf A data frame
#' @param biom_cols Character vector. Names of biomarker columns to combine
#' @param force Logical. If TRUE, bypasses the check for previous processing. Default is FALSE.
#'
#' @return Data frame with combined biomarker columns
#' @export
#'
#' @examples
#' biom_data <- data.frame(
#'   zscore_r3_nba = c(0.1, 0.2, NA),
#'   zscore_r5_nba = c(NA, 0.3, 0.4),
#'   zscore_r3_nhl = c(0.5, 0.6, NA),
#'   zscore_r5_nhl = c(NA, NA, 0.7)
#' )
#' combine_batch_biomarkers(biom_data, c("zscore_r5_nba", "zscore_r3_nba"))
combine_batch_biomarkers <- function(dataf, biom_cols, force = FALSE) {
  # Validate columns argument
  if (missing(biom_cols) || !is.character(biom_cols)) {
    stop("Columns argument must be a character vector specifying the columns to use.")
  }

  # Filter columns that match the specified columns
  missing_vars <- base::setdiff(biom_cols, colnames(dataf))
  if (length(missing_vars) > 0) {
    stop("Missing required columns: ", paste(missing_vars, collapse = ", "))
  }

  # Check if these columns have already been processed
  if (!force) {
    adRutils::is_processed("combine_batch_biomarkers", biom_cols, error_if_exists = TRUE)
  }

  # Group columns by biomarker name
  biomarker_groups <- list()
  for (col in biom_cols) {
    # Extract the batch identifier (r3, r5, etc.)
    batch_id <- stringr::str_extract(col, "r\\d+")
    if (is.na(batch_id)) {
      warning(paste("Cannot extract batch ID from column:", col, "- skipping"))
      next
    }
    # Extract biomarker name
    biom_name <- sub(".*_r\\d+_", "", col)
    # Initialize list for this biomarker if it doesn't exist
    if (!biom_name %in% names(biomarker_groups)) {
      biomarker_groups[[biom_name]] <- list()
    }
    # Add column to appropriate batch in this biomarker's group
    biomarker_groups[[biom_name]][[batch_id]] <- col
  }

  # Process each biomarker group
  for (biom_name in names(biomarker_groups)) {
    # Skip if we don't have at least two different batch versions
    if (length(biomarker_groups[[biom_name]]) < 2) {
      warning(paste("Only one batch found for", biom_name, "- skipping combination"))
      next
    }

    # New column name for combined data
    new_col_name <- paste0("zscore_", biom_name)

    # Check for column name collisions
    if (new_col_name %in% colnames(dataf)) {
      warning(paste("Column", new_col_name, "already exists. It will be overwritten."))
    }

    # Get the col names for this biomarker
    batch_cols <- unlist(biomarker_groups[[biom_name]])

    # Sort cols by batch ID to ensure consistent priority (e.g., r3 before r5)
    # This extracts the number from rX and sorts numerically
    batch_nums <- as.numeric(stringr::str_extract(names(batch_cols), "\\d+"))
    batch_cols <- batch_cols[order(batch_nums)]

    # combine columns with priority based on sorted order
    if (length(batch_cols) == 2) {
      dataf[[new_col_name]] <- dplyr::coalesce(dataf[[batch_cols[1]]], dataf[[batch_cols[2]]])
    } else {
      # For more than 2 columns, build a more complex coalesce call
      coalesce_expr <- paste0("dplyr::coalesce(",
                              paste(sapply(batch_cols, function(col) paste0("dataf[['", col, "']]")),
                                    collapse = ", "),")")
      dataf[[new_col_name]] <- eval(parse(text = coalesce_expr))
    }
  }

  # Register these columns as processed
  adRutils::register_processed("combine_batch_biomarkers", biom_cols)

  return(dataf)
}

#' @rdname standardize_biomarkers
#' @export
calc_zscore_biomarkers <- function(dataf, bioms, force = FALSE) {
  standardize_biomarkers(dataf, bioms, force)
}

#' @rdname combine_batch_biomarkers
#' @export
combine_zscore_bioms_by_batchID <- function(dataf, biom_cols, force = FALSE) {
  combine_batch_biomarkers(dataf, biom_cols, force)
}
