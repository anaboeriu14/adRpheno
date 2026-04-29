#' Pivot medication data from long to wide format
#'
#' Transforms medication data from long format (one row per medication) to
#' wide format (one row per participant) while preserving medication
#' positions and aggregating category indicators.
#'
#' @param dataf Long-format data frame.
#' @param id_col Participant ID column.
#' @param medication_col Medication name column.
#' @param position_col Column whose values encode the position of each
#'   medication (e.g. `"medication_1_name"`, `"medication_2_name"`). The
#'   leading integer is extracted to determine the order. **Required;
#'   no default.**
#' @param category_cols Optional binary indicator columns to aggregate. If
#'   supplied, also produces `total_*_meds` (count per participant) and
#'   `taking_*_med` (0/1 indicator) summary columns.
#' @param max_meds Maximum medications per participant (default: `NULL`,
#'   meaning all).
#' @param fill_value Value for missing medication slots (default: `NA`).
#' @param med_prefix Prefix for medication columns (default: `"new_"`).
#'
#' @return Wide-format data frame with one row per participant.
#' @export
#'
#' @details
#' Summary column names assume `category_cols` follow the `is_<name>_med`
#' convention produced by [categorize_drugs()]. For input
#' `is_diabetes_med`, the function generates `total_diabetes_meds` and
#' `taking_diabetes_med`. Columns not matching this pattern still work but
#' produce summary names that retain the original prefixes/suffixes.
#'
#' @examples
#' \dontrun{
#' wide_df <- pivot_medication_data_wide(
#'   long_df,
#'   id_col         = "participant_id",
#'   medication_col = "med_name",
#'   position_col   = "med_slot",
#'   category_cols  = c("is_diabetes_med", "is_hypertension_med")
#' )
#' }
pivot_medication_data_wide <- function(dataf,
                                       id_col,
                                       medication_col,
                                       position_col,
                                       category_cols = NULL,
                                       max_meds = NULL,
                                       fill_value = NA,
                                       med_prefix = "new_") {

  required_cols <- c(id_col, medication_col, position_col,
                     if (!is.null(category_cols)) category_cols)

  adRutils::validate_args(
    data           = dataf,
    columns        = required_cols,
    id_col         = adRutils::is_string(),
    medication_col = adRutils::is_string(),
    position_col   = adRutils::is_string(),
    med_prefix     = adRutils::is_string(),
    custom_checks  = list(
      list(condition = is.null(category_cols) || is.character(category_cols),
           message   = "{.arg category_cols} must be NULL or a character vector"),
      list(condition = is.null(max_meds) ||
             (is.numeric(max_meds) && length(max_meds) == 1L && max_meds > 0),
           message   = "{.arg max_meds} must be NULL or a single positive number")
    )
  )

  numbered_data <- .extract_med_positions(dataf, id_col, position_col, med_prefix)

  if (!is.null(max_meds)) {
    numbered_data <- numbered_data[numbered_data$med_num <= max_meds, ]
  }

  wide_meds <- .pivot_medications(numbered_data, id_col, medication_col, fill_value)

  if (!is.null(category_cols)) {
    wide_meds <- .pivot_categories(wide_meds, numbered_data, id_col,
                                   category_cols, fill_value)
  }

  all_ids <- unique(dataf[[id_col]])
  .ensure_all_ids(wide_meds, all_ids, id_col)
}


# ---- internal helpers ------------------------------------------------------

#' Extract medication slot numbers and build per-row output column names
#' @keywords internal
#' @noRd
.extract_med_positions <- function(dataf, id_col, position_col, med_prefix) {
  dataf %>%
    dplyr::mutate(
      !!id_col     := as.character(.data[[id_col]]),
      med_num      = as.numeric(stringr::str_extract(.data[[position_col]], "\\d+")),
      med_num      = dplyr::coalesce(.data$med_num, dplyr::row_number()),
      new_col_name = paste0(med_prefix, .data[[position_col]])
    )
}

#' Pivot medication names long -> wide
#' @keywords internal
#' @noRd
.pivot_medications <- function(numbered_data, id_col, medication_col, fill_value) {
  numbered_data %>%
    dplyr::select(dplyr::all_of(c(id_col, medication_col)), "new_col_name") %>%
    tidyr::pivot_wider(
      names_from   = "new_col_name",
      values_from  = dplyr::all_of(medication_col),
      values_fill  = fill_value
    )
}

#' Pivot category indicators wide and add per-participant summaries
#' @keywords internal
#' @noRd
.pivot_categories <- function(wide_meds, numbered_data, id_col, category_cols, fill_value) {
  for (cat_col in category_cols) {
    wide_cat <- numbered_data %>%
      dplyr::select(dplyr::all_of(c(id_col, cat_col)), "med_num") %>%
      tidyr::pivot_wider(
        names_from   = "med_num",
        values_from  = dplyr::all_of(cat_col),
        names_prefix = paste0(cat_col, "_"),
        values_fill  = fill_value
      )

    wide_meds <- dplyr::left_join(wide_meds, wide_cat, by = id_col)
  }

  .add_category_summaries(wide_meds, category_cols)
}

#' Add total_* and taking_* summary columns for each category
#' @keywords internal
#' @noRd
.add_category_summaries <- function(wide_meds, category_cols) {
  for (cat_col in category_cols) {
    cat_pattern <- paste0("^", cat_col, "_\\d+$")
    cat_columns <- grep(cat_pattern, names(wide_meds), value = TRUE)

    if (length(cat_columns) > 0L) {
      stripped <- gsub("^is_|_med$", "", cat_col)
      total_col  <- paste0("total_",  stripped, "_meds")
      binary_col <- paste0("taking_", stripped, "_med")

      wide_meds[[total_col]]  <- rowSums(wide_meds[cat_columns], na.rm = TRUE)
      wide_meds[[binary_col]] <- as.integer(wide_meds[[total_col]] > 0)
    }
  }

  wide_meds
}

#' Left-join the wide result onto the full set of participant IDs to ensure
#' rows are kept for participants who had no medications recorded
#' @keywords internal
#' @noRd
.ensure_all_ids <- function(wide_meds, all_ids, id_col) {
  all_ids_df <- data.frame(id = all_ids, stringsAsFactors = FALSE)
  names(all_ids_df) <- id_col

  dplyr::left_join(all_ids_df, wide_meds, by = id_col)
}
