#' Pivot medication data from long to wide format
#'
#' Transforms medication data from long format (one row per medication) to wide
#' format (one row per participant) while preserving medication positions and
#' aggregating category indicators.
#'
#' @param dataf Long-format data frame
#' @param id_col Participant ID column
#' @param medication_col Medication name column
#' @param position_col Column with position identifiers (e.g., "medication_1_name")
#' @param category_cols Binary indicator columns to aggregate (optional)
#' @param max_meds Maximum medications per participant (default: NULL = all)
#' @param fill_value Value for missing medications (default: NA)
#' @param med_prefix Prefix for medication columns (default: "new_")
#'
#' @return Wide-format data frame with one row per participant
#' @export
#'
#' @examples
#' \dontrun{
#' wide_df <- pivot_medication_data_wide(
#'   long_df,
#'   id_col = "participant_id",
#'   medication_col = "med_name",
#'   category_cols = c("is_diabetes_med", "is_hypertension_med")
#' )
#' }
pivot_medication_data_wide <- function(dataf,
                                       id_col,
                                       medication_col,
                                       position_col = "question_col",
                                       category_cols = NULL,
                                       max_meds = NULL,
                                       fill_value = NA,
                                       med_prefix = "new_") {

  .validate_pivot_inputs(dataf, id_col, medication_col, position_col, category_cols)

  # Prepare data with positions
  numbered_data <- .extract_med_positions(dataf, id_col, position_col, med_prefix)

  # Apply max_meds filter
  if (!is.null(max_meds)) {
    numbered_data <- numbered_data[numbered_data$med_num <= max_meds, ]
  }

  # Pivot medications
  wide_meds <- .pivot_medications(numbered_data, id_col, medication_col, fill_value)

  # Pivot and summarize categories
  if (!is.null(category_cols)) {
    wide_meds <- .pivot_categories(wide_meds, numbered_data, id_col, category_cols, fill_value)
  }

  # Ensure all participants included
  all_ids <- unique(dataf[[id_col]])
  .ensure_all_ids(wide_meds, all_ids, id_col)
}

#' @keywords internal
#' @noRd
.validate_pivot_inputs <- function(dataf, id_col, medication_col, position_col, category_cols) {
  if (!is.data.frame(dataf) || nrow(dataf) == 0) {
    cli::cli_abort("Input must be a non-empty data frame")
  }

  required <- c(id_col, medication_col, position_col)
  if (!is.null(category_cols)) required <- c(required, category_cols)

  missing <- setdiff(required, names(dataf))
  if (length(missing) > 0) {
    cli::cli_abort("Column{?s} not found: {.val {missing}}")
  }
}

#' @keywords internal
#' @noRd
.extract_med_positions <- function(dataf, id_col, position_col, med_prefix) {
  dataf %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      med_num = as.numeric(stringr::str_extract(.data[[position_col]], "\\d+")),
      med_num = dplyr::coalesce(.data$med_num, dplyr::row_number()),
      new_col_name = paste0(med_prefix, .data[[position_col]])
    )
}


#' @keywords internal
#' @noRd
.pivot_medications <- function(numbered_data, id_col, medication_col, fill_value) {
  numbered_data %>%
    dplyr::select(dplyr::all_of(c(id_col, medication_col)), "new_col_name") %>%
    tidyr::pivot_wider(
      names_from = "new_col_name",
      values_from = dplyr::all_of(medication_col),
      values_fill = fill_value
    )
}


#' @keywords internal
#' @noRd
.pivot_categories <- function(wide_meds, numbered_data, id_col, category_cols, fill_value) {
  for (cat_col in category_cols) {
    wide_cat <- numbered_data %>%
      dplyr::select(dplyr::all_of(c(id_col, cat_col)), "med_num") %>%
      tidyr::pivot_wider(
        names_from = "med_num",
        values_from = dplyr::all_of(cat_col),
        names_prefix = paste0(cat_col, "_"),
        values_fill = fill_value
      )

    wide_meds <- dplyr::left_join(wide_meds, wide_cat, by = id_col)
  }

  # Add summary columns
  .add_category_summaries(wide_meds, category_cols)
}


#' @keywords internal
#' @noRd
.add_category_summaries <- function(wide_meds, category_cols) {
  for (cat_col in category_cols) {
    cat_pattern <- paste0("^", cat_col, "_\\d+$")
    cat_columns <- grep(cat_pattern, names(wide_meds), value = TRUE)

    if (length(cat_columns) > 0) {
      # Total count
      total_col <- paste0("total_", gsub("^is_|_med$", "", cat_col), "_meds")
      wide_meds[[total_col]] <- rowSums(wide_meds[cat_columns], na.rm = TRUE)

      # Binary indicator
      binary_col <- paste0("taking_", gsub("^is_|_med$", "", cat_col), "_med")
      wide_meds[[binary_col]] <- as.integer(wide_meds[[total_col]] > 0)
    }
  }

  return(wide_meds)
}


#' @keywords internal
#' @noRd
.ensure_all_ids <- function(wide_meds, all_ids, id_col) {
  all_ids_df <- data.frame(id = all_ids, stringsAsFactors = FALSE)
  names(all_ids_df) <- id_col

  dplyr::left_join(all_ids_df, wide_meds, by = id_col)
}
