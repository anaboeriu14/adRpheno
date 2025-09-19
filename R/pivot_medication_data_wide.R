#' Transform long medication data to wide format with position preservation
#'
#' Converts medication data from long format to wide format while preserving original
#' medication positions and providing the option to include binary medication category indicators.
#'
#' @param dataf Data frame in long format containing medication data
#' @param id_col Character. Name of the ID column identifying participants
#' @param medication_col Character. Name of the column containing medication names/values
#' @param position_col Character. Name of the column containing position identifiers
#'   (e.g., "medication_1_name", "medication_2_name"). Default is "question_col"
#' @param category_cols Character vector. Names of binary indicator columns to include
#'   in wide format (optional). These will be numbered sequentially (e.g., "is_diabetes_med_1")
#' @param max_meds Integer. Maximum number of medication positions to include per
#'   participant. If NULL (default), includes all positions found in data
#' @param fill_value Value to use for missing medications. Default is NA
#' @param med_prefix Character. Prefix to add to medication column names. Default is "new_"
#'   which transforms "medication_1_name" to "new_medication_1_name"
#'
#' @return Data frame in wide format with:
#'   \itemize{
#'     \item One row per participant
#'     \item Medication columns named with med_prefix + original position column values
#'     \item Category columns (if specified) named with category name + position number
#'     \item All participants included, even those without medication data
#'   }
#'
#' @details
#' Function preserves the original positioning of medications by extracting position
#' numbers from the position_col and maintaining those positions in the wide format.
#' Ensures that medication_5_name always becomes new_medication_5_name regardless
#' of whether medications 1-4 contain data or are NA.
#'
#' Binary category indicators are handled separately and use simple numeric suffixes
#' (e.g., is_diabetes_med_1, is_diabetes_med_2) based on the extracted position numbers.
#'
#' @examples
#' \dontrun{
#'
#' # Include binary category indicators
#' wide_meds <- create_wide_medication_matrix(
#'   data = medication_long_data,
#'   id_col = "participant_id",
#'   medication_col = "medication_col_to_use",
#'   category_cols = c("diabetes_med", "hypertension_med"),
#'   med_prefix = "clean_"
#' )
#'
#' # Limit to first 10 medications only
#' wide_meds <- create_wide_medication_matrix(
#'   data = medication_long_data,
#'   id_col = "participant_id",
#'   medication_col = "medication_name",
#'   max_meds = 10
#' )
#' }
#'
#' @export
pivot_medication_data_wide <- function(dataf,
                                          id_col,
                                          medication_col,
                                          position_col = "question_col",
                                          category_cols = NULL,
                                          max_meds = NULL,
                                          fill_value = NA,
                                          med_prefix = "new_") {

  # Build required columns list
  required_cols <- c(id_col, medication_col, position_col)
  if (!is.null(category_cols)) {
    required_cols <- c(required_cols, category_cols)
  }

  # Input validation
  validate_params(
    data = dataf,
    columns = required_cols,
    context = "create_wide_medication_matrix",
    custom_checks = list(
      list(
        condition = is.character(id_col) && length(id_col) == 1,
        message = "{.arg id_col} must be a single character string"
      ),
      list(
        condition = is.character(medication_col) && length(medication_col) == 1,
        message = "{.arg medication_col} must be a single character string"
      ),
      list(
        condition = is.character(position_col) && length(position_col) == 1,
        message = "{.arg position_col} must be a single character string"
      ),
      list(
        condition = is.null(category_cols) || is.character(category_cols),
        message = "{.arg category_cols} must be NULL or a character vector"
      ),
      list(
        condition = is.null(max_meds) || (is.numeric(max_meds) && max_meds > 0),
        message = "{.arg max_meds} must be NULL or a positive integer"
      ),
      list(
        condition = is.character(med_prefix) && length(med_prefix) == 1,
        message = "{.arg med_prefix} must be a single character string"
      )
    )
  )

  # Prepare data - keep everything, extract position
  numbered_data <- dataf %>%
    select(all_of(required_cols)) %>%
    group_by(!!sym(id_col)) %>%
    mutate(
      !!id_col := as.character(.data[[id_col]]),
      position = as.numeric(stringr::str_extract(.data[[position_col]], "\\d+")),
      # Add prefix to the beginning of the position column value
      new_col_name = paste0(med_prefix, .data[[position_col]]),
      # Handle cases where no number is found - use row number within group
      position = dplyr::coalesce(.data[["position"]], dplyr::row_number())
    ) %>%
    ungroup() %>%
    rename(med_num = .data[["position"]])

  # Apply max_meds filter if specified
  if (!is.null(max_meds)) {
    numbered_data <- numbered_data %>%
      filter(.data[["med_num"]] <= max_meds)

  }

  # Create wide format for medications
  wide_meds <- numbered_data %>%
    select(all_of(c(id_col, medication_col)), .data[["new_col_name"]]) %>%
    pivot_wider(
      names_from =  .data[["new_col_name"]],
      values_from = all_of(medication_col),
      values_fill = fill_value
    )

  # Create wide format for categories if specified
  if (!is.null(category_cols)) {
    wide_categories <- list()

    for (cat_col in category_cols) {
      wide_cat <- numbered_data %>%
        select(all_of(c(id_col, cat_col)), .data[["med_num"]]) %>%
        pivot_wider(
          names_from = .data[["med_num"]],
          values_from = all_of(cat_col),
          names_prefix = paste0(cat_col, "_"),
          values_fill = fill_value
        )

      wide_categories[[cat_col]] <- wide_cat
    }

    # Join all category matrices
    result <- wide_meds
    for (cat_data in wide_categories) {
      result <- result %>%
        left_join(cat_data, by = id_col)
    }
  } else {
    result <- wide_meds
  }

  # Ensure all participants are included
  all_ids <- dataf %>% distinct(.data[[id_col]])

  if (!is.null(category_cols)) {
    for (cat_col in category_cols) {
      cat_pattern <- paste0("^", cat_col, "_")
      cat_columns <- grep(cat_pattern, names(result), value = TRUE)

      if (length(cat_columns) > 0) {
        # Calculate total count (sum of 1s, treating NA as 0 for counting)
        total_col <- paste0("total_", gsub("is_", "", cat_col), "s")
        result[[total_col]] <- rowSums(result[cat_columns], na.rm = TRUE)

        # Calculate binary indicator (any medication in this category)
        binary_col <- paste0("taking_",  gsub("is_", "", cat_col), "s")
        result[[binary_col]] <- as.numeric(result[[total_col]] > 0)
      }
    }
  }

  final_result <- all_ids %>%
    left_join(result, by = id_col)

  return(final_result)
}
