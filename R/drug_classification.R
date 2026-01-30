#' Categorize medications by therapeutic class
#'
#' Adds binary indicator columns for medication categories based on pattern
#' matching in medication names and/or ATC codes.
#'
#' @param dataf Data frame with medication data
#' @param med_col Column with medication names
#' @param atc_col Column with ATC codes/names (optional)
#' @param categories Named list of patterns for each category (see Details)
#' @param prefix Prefix for output columns (default: "is_")
#' @param suffix Suffix for output columns (default: "_med")
#'
#' @return Data frame with binary category columns added
#' @export
#'
#' @details
#' Each category is a character vector of patterns to match (case-insensitive).
#' Patterns are matched as whole words using word boundaries.
#'
#' Default categories are:
#' - **cholesterol**: lipid modifying agents, statin(s)
#' - **diabetes**: drugs used in diabetes
#' - **hypertension**: renin-angiotensin, calcium channel blockers, diuretics, beta blockers
#'
#' @examples
#' \dontrun{
#' # Use defaults
#' df <- categorize_drugs(df, med_col = "medication", atc_col = "atc2")
#'
#' my_cats <- list(
#'   anticoagulant = c("warfarin", "apixaban", "rivaroxaban"),
#'   antidepressant = c("ssri", "snri", "sertraline", "fluoxetine")
#' )
#' df <- categorize_drugs(df, med_col = "medication", categories = my_cats)
#' }
categorize_drugs <- function(dataf,
                             med_col,
                             atc_col = NULL,
                             categories = NULL,
                             prefix = "is_",
                             suffix = "_med") {

  .validate_categorize_inputs(dataf, med_col, atc_col, categories)

  if (is.null(categories)) {
    categories <- .get_default_categories()
  }

  for (cat_name in names(categories)) {
    col_name <- paste0(prefix, cat_name, suffix)
    patterns <- categories[[cat_name]]

    dataf[[col_name]] <- .match_category(dataf, med_col, atc_col, patterns)
  }

  return(dataf)
}

#' @keywords internal
#' @noRd
.validate_categorize_inputs <- function(dataf, med_col, atc_col, categories) {
  if (!is.data.frame(dataf) || nrow(dataf) == 0) {
    cli::cli_abort("Input must be a non-empty data frame")
  }
  if (!med_col %in% names(dataf)) {
    cli::cli_abort("Column '{med_col}' not found")
  }
  if (!is.null(atc_col) && !atc_col %in% names(dataf)) {
    cli::cli_abort("Column '{atc_col}' not found")
  }
  if (!is.null(categories) && (!is.list(categories) || is.null(names(categories)))) {
    cli::cli_abort("{.arg categories} must be a named list")
  }
}


#' @keywords internal
#' @noRd
.get_default_categories <- function() {
  list(
    cholesterol = c(
      "lipid modifying agents",
      "statin", "statins",
      "hmg-coa reductase inhibitor"
    ),
    diabetes = c(
      "drugs used in diabetes",
      "blood glucose lowering",
      "antidiabetic"
    ),
    hypertension = c(
      "agents acting on the renin-angiotensin system",
      "calcium channel blockers",
      "beta blocking agents",
      "diuretics",
      "antihypertensives", "antihypertensive"
    )
  )
}


#' @keywords internal
#' @noRd
.match_category <- function(dataf, med_col, atc_col, patterns) {
  # Build regex: match any pattern as whole word
  regex <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\b")

  # Match in med_col
  med_match <- grepl(regex, dataf[[med_col]], ignore.case = TRUE)
  med_match[is.na(dataf[[med_col]])] <- FALSE

  # Match in atc_col if provided
  if (!is.null(atc_col)) {
    atc_match <- grepl(regex, dataf[[atc_col]], ignore.case = TRUE)
    atc_match[is.na(dataf[[atc_col]])] <- FALSE
    return(as.integer(med_match | atc_match))
  }

  as.integer(med_match)
}
