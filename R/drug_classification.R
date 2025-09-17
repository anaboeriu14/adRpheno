#' Categorize medications based on patterns
#'
#' Flexible function to categorize medications into custom categories based on
#' pattern matching in medication names and/or ATC codes.
#'
#' @param dataf A data frame containing medication data
#' @param med_col Character. Column name containing medication names
#' @param atc_col Character. Column name containing ATC codes (optional)
#' @param categories Named list. Each element contains patterns for a medication category
#' @param check_both Logical. Check both med_col and atc_col (default: TRUE if atc_col provided)
#' @param prefix Character. Prefix for output columns (default: "")
#' @param suffix Character. Suffix for output columns (default: "_med")
#'
#' @return Data frame with additional binary columns for each category
#' @export
#'
#' @examples
#' \dontrun{
#' # Define categories
#' med_categories <- list(
#'   cholesterol = c("lipid modifying agents", "statin", "statins"),
#'   diabetes = c("drugs used in diabetes"),
#'   glp1 = c("semaglutide", "ozempic", "liraglutide", "victoza")
#' )
#'
#' # Categorize medications
#' df_categorized <- categorize_drugs(
#'   dataf = my_data,
#'   med_col = "medication_name",
#'   atc_col = "atc2_code",
#'   categories = med_categories
#' )
#' }
categorize_drugs <- function(dataf, med_col, atc_col = NULL, categories,
                                   check_both = TRUE, prefix = "is_", suffix = "_med") {

  # === STEP 1: Validate inputs ===
  adRutils::validate_params(
    data = dataf,
    columns = med_col,
    custom_checks = list(
      list(
        condition = is.list(categories) && length(categories) > 0 &&
          all(sapply(categories, is.character)),
        message = "categories must be a named list of character vectors"
      ),
      list(
        condition = is.null(names(categories)) || all(names(categories) != ""),
        message = "categories list must have non-empty names"
      ),
      list(
        condition = is.character(prefix) && length(prefix) == 1,
        message = "prefix must be a single character string"
      ),
      list(
        condition = is.character(suffix) && length(suffix) == 1,
        message = "suffix must be a single character string"
      )
    ),
    context = "categorize_medications"
  )

  # Check ATC column if provided
  if (!is.null(atc_col)) {
    if (!atc_col %in% names(dataf)) {
      cli_abort("ATC column {atc_col} not found in dataframe")
    }
  }

  # === STEP 2: Process each category ===
  result_df <- dataf

  for (category_name in names(categories)) {
    patterns <- categories[[category_name]]
    col_name <- paste0(prefix, category_name, suffix)

    # Create regex pattern
    pattern_regex <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\b")

    # Check medication names
    med_match <- grepl(pattern_regex, result_df[[med_col]], ignore.case = TRUE)
    med_match[is.na(result_df[[med_col]])] <- FALSE

    if (check_both && !is.null(atc_col)) {
      # Check both medication names and ATC codes
      atc_match <- grepl(pattern_regex, result_df[[atc_col]], ignore.case = TRUE)
      atc_match[is.na(result_df[[atc_col]])] <- FALSE

      # Combine: 1 if either matches, 0 otherwise
      result_df[[col_name]] <- as.integer(med_match | atc_match)

    } else {
      # Check only medication names
      result_df[[col_name]] <- as.integer(med_match)
    }

    # Report count
    count <- sum(result_df[[col_name]], na.rm = TRUE)
    cli_alert_success("Found {count} {category_name} medications")
  }

  return(result_df)
}
