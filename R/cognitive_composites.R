#' Compute Adjusted Composite Scores
#'
#' Computes z-scores for test scores adjusted for demographic factors and
#' calculates composite scores for specified test groups.
#'
#' @param dataf A data frame containing raw test scores and demographic information
#' @param test_groups Named list of character vectors, one per composite group.
#'   Each element names the columns to include in that group's composite.
#' @param grouping_vars Character vector of demographic grouping columns
#' @param filters Optional named list of string filter expressions evaluated
#'   against `dataf` (e.g. `list(diagnosis = "diagnosis == 'CN'")`)
#' @param digits Integer for decimal places (default: 3)
#' @param verbose Logical. Show informative messages (default: `TRUE`)
#'
#' @return Data frame with added z-score columns (`zscore_{var}_{group}`) and
#'   composite columns (`{group}_comp_score`).
#' @export
#'
#' @examples
#' \dontrun{
#' test_groups <- list(
#'   memory    = c("LM1", "LM2", "SEVLT_imm"),
#'   executive = c("digit_span", "trails_a")
#' )
#'
#' result <- create_adjusted_composites(
#'   dataf         = my_data,
#'   test_groups   = test_groups,
#'   grouping_vars = c("age_group", "edu_group")
#' )
#' }
create_adjusted_composites <- function(dataf, test_groups, grouping_vars,
                                       filters = NULL, digits = 3, verbose = TRUE) {

  all_tests <- .validate_test_groups(test_groups, dataf)

  adRutils::validate_args(
    data            = dataf,
    columns         = c(grouping_vars, all_tests),
    numeric_columns = all_tests,
    grouping_vars   = adRutils::is_nonempty_character(),
    digits          = adRutils::is_number(min = 0),
    verbose         = adRutils::is_flag(),
    custom_checks   = list(
      list(condition = is.null(filters) || is.list(filters),
           message   = "{.arg filters} must be NULL or a named list")
    )
  )

  if (verbose) {
    cli::cli_alert_info("Creating adjusted composites for {length(test_groups)} test group{?s}")
  }

  result_df <- dataf %>%
    .apply_filters(filters) %>%
    .compute_cognitive_zscores(test_groups, grouping_vars) %>%
    .compute_cognitive_composites(test_groups) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., digits)))

  if (verbose) {
    cli::cli_alert_success("Created {length(test_groups)} composite score{?s}")
  }

  result_df
}

#' Combines multiple test component scores into one column using either a
#' sum or a mean.
#'
#' @param dataf A data frame containing test component columns
#' @param component_cols Character vector of column names to combine
#' @param result_col Name for new score column (default: `"total_score"`)
#' @param method Combination method: `"sum"` or `"mean"` (default: `"sum"`)
#' @param na.rm Remove `NA` values when combining (default: `TRUE`)
#' @param verbose Show informative messages (default: `TRUE`)
#'
#' @return Data frame with the added combined-score column
#' @export
#'
#' @examples
#' \dontrun{
#' # Sum of two Trail Making Test times
#' result <- combine_cognitive_test_components(
#'   dataf          = my_data,
#'   component_cols = c("trails_a_time", "trails_b_time"),
#'   result_col     = "trails_total"
#' )
#'
#' # Mean of several memory tests
#' result <- combine_cognitive_test_components(
#'   dataf          = my_data,
#'   component_cols = c("recall_1", "recall_2", "recognition"),
#'   result_col     = "memory_mean",
#'   method         = "mean"
#' )
#' }
combine_cognitive_test_components <- function(dataf, component_cols,
                                              result_col = "total_score",
                                              method = "sum", na.rm = TRUE,
                                              verbose = TRUE) {

  adRutils::validate_args(
    data            = dataf,
    columns         = component_cols,
    numeric_columns = component_cols,
    component_cols  = adRutils::is_nonempty_character(),
    result_col      = adRutils::is_string(),
    method          = adRutils::is_one_of(c("sum", "mean")),
    na.rm           = adRutils::is_flag(),
    verbose         = adRutils::is_flag()
  )

  if (result_col %in% names(dataf) && verbose) {
    cli::cli_alert_info("Column '{result_col}' already exists and will be overwritten")
  }

  if (verbose) {
    cli::cli_alert_info(
      "Creating '{result_col}' using {method} of {length(component_cols)} component{?s}"
    )
  }

  aggregate_fn <- if (method == "sum") rowSums else rowMeans

  result <- dataf %>%
    dplyr::mutate(
      !!result_col := aggregate_fn(
        dplyr::across(dplyr::all_of(component_cols)),
        na.rm = na.rm
      )
    )

  if (verbose) {
    cli::cli_alert_success("Created column '{result_col}'")
  }

  result
}




# ---- internal helpers ------------------------------------------------------

#' Validate test groups structure and column existence
#' @keywords internal
#' @noRd
.validate_test_groups <- function(test_groups, dataf) {
  if (!is.list(test_groups) || is.null(names(test_groups))) {
    cli::cli_abort("{.arg test_groups} must be a named list of character vectors")
  }

  all_tests <- unlist(test_groups)
  missing_cols <- setdiff(all_tests, colnames(dataf))

  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing test column{?s}: {.val {missing_cols}}")
  }

  all_tests
}

#' Apply string-expression filters to a data frame
#' @keywords internal
#' @noRd
.apply_filters <- function(dataf, filters) {
  if (is.null(filters)) return(dataf)

  for (filter_var in names(filters)) {
    filter_expr <- filters[[filter_var]]
    dataf <- dataf[eval(parse(text = filter_expr), dataf), ]
  }

  dataf
}

#' Note: this is intentionally separate from the public `compute_zscores()`
#' (now in adRutils). The output naming `zscore_{var}_{group_name}` is
#' required by `create_adjusted_composites()` to disambiguate when the same
#' test column appears in multiple test groups; `compute_zscores()` uses a
#' single-prefix scheme that would collide in that case.
#' @keywords internal
#' @noRd
.compute_cognitive_zscores <- function(dataf, test_groups, grouping_vars) {
  result_df <- dataf %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars)))

  for (group_name in names(test_groups)) {
    cols <- test_groups[[group_name]]
    suffix <- paste0("_", group_name)

    result_df <- result_df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols),
          ~as.vector(scale(.x)),
          .names = "zscore_{.col}{suffix}"
        )
      )
  }

  result_df %>% dplyr::ungroup()
}

#' Compute composite scores from per-group z-scores
#' @keywords internal
#' @noRd
.compute_cognitive_composites <- function(dataf, test_groups) {
  result_df <- dataf

  for (group_name in names(test_groups)) {
    composite_name <- paste0(group_name, "_comp_score")
    zscore_cols    <- paste0("zscore_", test_groups[[group_name]], "_", group_name)
    result_df[[composite_name]] <- rowMeans(result_df[zscore_cols], na.rm = TRUE)
  }

  result_df
}
