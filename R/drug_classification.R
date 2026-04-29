#' Categorize medications by therapeutic class
#'
#' Adds binary indicator columns for medication categories based on pattern
#' matching in medication names and/or ATC codes.
#'
#' Patterns are matched as whole words (`\b...\b`) using `grepl()` with
#' `ignore.case = TRUE`. Patterns are interpreted as **regular expressions**:
#' regex metacharacters (`.`, `+`, `*`, `(`, `)`, `[`, `]`, `?`, `^`, `$`,
#' `|`, `\\`) inside a pattern have their usual regex meaning. To match a
#' literal metacharacter, escape it (e.g. `"hmg-coa\\."`). Plain word
#' patterns like `"statin"` need no escaping.
#'
#' @param dataf Data frame with medication data.
#' @param med_col Column with medication names.
#' @param atc_col Column with ATC codes/names. Optional; if supplied, a
#'   match in either `med_col` or `atc_col` flags the row.
#' @param categories Named list of pattern vectors. If `NULL` (default), uses
#'   built-in cardiometabolic categories (cholesterol, diabetes, hypertension).
#'   Supply your own list for any other therapeutic grouping.
#' @param prefix Prefix for output column names (default: `"is_"`).
#' @param suffix Suffix for output column names (default: `"_med"`).
#'
#' @return The input data frame with one binary (0/1) column per category.
#' @export
#'
#' @details
#' The built-in defaults assume `atc_col` (when supplied) contains
#' second-level ATC class names, i.e. the output of [add_atc_classification()]
#' with `atc_level = "second"`. They mix exact ATC2 class names with common
#' generic-name patterns so they match against either column.
#'
#' Default categories:
#' - **cholesterol**: lipid modifying agents, statins
#' - **diabetes**: drugs used in diabetes, blood glucose lowering agents,
#'   antidiabetics
#' - **hypertension**: renin-angiotensin agents, calcium channel blockers,
#'   diuretics, beta blockers, antihypertensives
#'
#' For other therapeutic groups, or for matching against a different ATC
#' level, supply `categories` explicitly.
#'
#' @examples
#' \dontrun{
#' # Use defaults (cardiometabolic, ATC2-aware)
#' df <- categorize_drugs(df, med_col = "medication", atc_col = "atc2_class")
#'
#' # Custom categories
#' my_cats <- list(
#'   anticoagulant  = c("antithrombotic agents", "warfarin", "apixaban", "rivaroxaban"),
#'   antidepressant = c("antidepressants", "ssri", "sertraline", "fluoxetine")
#' )
#' df <- categorize_drugs(df, med_col = "medication", categories = my_cats)
#' }
categorize_drugs <- function(dataf,
                             med_col,
                             atc_col = NULL,
                             categories = NULL,
                             prefix = "is_",
                             suffix = "_med") {

  required_cols <- c(med_col, if (!is.null(atc_col)) atc_col)

  adRutils::validate_args(
    data          = dataf,
    columns       = required_cols,
    med_col       = adRutils::is_string(),
    prefix        = adRutils::is_string(),
    suffix        = adRutils::is_string(),
    custom_checks = list(
      list(condition = is.null(atc_col) || (is.character(atc_col) && length(atc_col) == 1L),
           message   = "{.arg atc_col} must be NULL or a single string"),
      list(condition = is.null(categories) || (is.list(categories) && !is.null(names(categories))),
           message   = "{.arg categories} must be NULL or a named list")
    )
  )

  if (is.null(categories)) {
    categories <- .get_default_drug_categories()
  }

  for (cat_name in names(categories)) {
    col_name <- paste0(prefix, cat_name, suffix)
    dataf[[col_name]] <- .match_category(dataf, med_col, atc_col,
                                         categories[[cat_name]])
  }

  dataf
}


#' Built-in default cardiometabolic categories (ATC2-aware)
#'
#' Patterns target second-level ATC class names where possible, with
#' generic-name fallbacks for matches against medication-name columns.
#' @keywords internal
#' @noRd
.get_default_drug_categories <- function() {
  list(
    cholesterol = c(
      "lipid modifying agents",
      "statin", "statins"
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

#' Match a set of patterns across one or two text columns
#' @keywords internal
#' @noRd
.match_category <- function(dataf, med_col, atc_col, patterns) {
  regex <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\b")

  med_match <- grepl(regex, dataf[[med_col]], ignore.case = TRUE)
  med_match[is.na(dataf[[med_col]])] <- FALSE

  if (is.null(atc_col)) return(as.integer(med_match))

  atc_match <- grepl(regex, dataf[[atc_col]], ignore.case = TRUE)
  atc_match[is.na(dataf[[atc_col]])] <- FALSE

  as.integer(med_match | atc_match)
}
