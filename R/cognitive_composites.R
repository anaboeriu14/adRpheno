# Exported functions -------------------------------------------------------

#' Compute Adjusted Composite Scores
#'
#' Computes z-scores for test scores adjusted for demographic factors and
#' calculates composite scores for specified test groups.
#'
#' @param dataf A data frame containing raw test scores and demographic information
#' @param test_groups Named list of character vectors for each test group
#' @param grouping_vars Character vector of demographic grouping columns
#' @param filters Named list of filtering conditions (optional)
#' @param digits Integer for decimal places (default: 3)
#' @param verbose Logical. Show informative messages (default: TRUE)
#'
#' @return Data frame with z-scores and composite scores
#' @export
#'
#' @examples
#' \dontrun{
#' test_groups <- list(
#'   memory = c("LM1", "LM2", "SEVLT_imm"),
#'   executive = c("digit_span", "trails_a")
#' )
#'
#' result <- create_adjusted_composites(
#'   dataf = my_data,
#'   test_groups = test_groups,
#'   grouping_vars = c("age_group", "edu_group")
#' )
#' }
create_adjusted_composites <- function(dataf, test_groups, grouping_vars,
                                       filters = NULL, digits = 3, verbose = TRUE) {

  # Validate inputs
  all_tests <- .validate_test_groups(test_groups, dataf)

  adRutils::validate_params(
    data = dataf,
    columns = c(grouping_vars, all_tests),
    numeric_columns = all_tests,
    custom_checks = list(
      list(condition = is.numeric(digits) && length(digits) == 1 && digits >= 0,
           message = "{.arg digits} must be a single non-negative number"),
      list(condition = is.logical(verbose) && length(verbose) == 1,
           message = "{.arg verbose} must be TRUE or FALSE"),
      list(condition = is.null(filters) || is.list(filters),
           message = "{.arg filters} must be NULL or a named list")
    ),
    context = "create_adjusted_composites"
  )

  # Check if outputs already exist
  expected_outputs <- .get_expected_composite_outputs(test_groups)
  if (.outputs_exist(dataf, expected_outputs) && verbose) {
    cli::cli_alert_info("Composite score outputs already exist. Results may be overwritten.")
  }

  if (verbose) {
    cli::cli_alert_info("Creating adjusted composites for {length(test_groups)} test group{?s}")
  }

  # Apply filters and compute
  result_df <- dataf %>%
    .apply_filters(filters) %>%
    .compute_grouped_zscores(test_groups, grouping_vars) %>%
    .compute_composite_scores(test_groups) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., digits)))

  if (verbose) {
    cli::cli_alert_success("Created {length(test_groups)} composite score{?s}")
  }

  return(result_df)
}


#' Calculate Total Score from Multiple Test Components
#'
#' Combines multiple test component scores using sum or mean.
#'
#' @param dataf A data frame containing test component columns
#' @param component_cols Character vector of column names to combine
#' @param result_col Name for new total score column (default: "total_score")
#' @param method Combination method: "sum" or "mean" (default: "sum")
#' @param na.rm Remove NA values when combining (default: TRUE)
#' @param verbose Show informative messages (default: TRUE)
#'
#' @return Data frame with added total score column
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sum_cognitive_test_components(
#'   dataf = my_data,
#'   component_cols = c("trails_a_time", "trails_b_time"),
#'   result_col = "trails_total"
#' )
#' }
sum_cognitive_test_components <- function(dataf, component_cols,
                                          result_col = "total_score",
                                          method = "sum", na.rm = TRUE,
                                          verbose = TRUE) {

  adRutils::validate_params(
    data = dataf,
    columns = component_cols,
    numeric_columns = component_cols,
    custom_checks = list(
      list(condition = method %in% c("sum", "mean"),
           message = "{.arg method} must be 'sum' or 'mean'"),
      list(condition = is.logical(na.rm) && length(na.rm) == 1,
           message = "{.arg na.rm} must be TRUE or FALSE"),
      list(condition = is.logical(verbose) && length(verbose) == 1,
           message = "{.arg verbose} must be TRUE or FALSE"),
      list(condition = is.character(result_col) && length(result_col) == 1,
           message = "{.arg result_col} must be a single character string")
    ),
    context = "sum_cognitive_test_components"
  )

  # Check if output already exists
  if (result_col %in% names(dataf) && verbose) {
    cli::cli_alert_info("Column '{result_col}' already exists and will be overwritten.")
  }

  if (verbose) {
    cli::cli_alert_info("Creating '{result_col}' using {method} of {length(component_cols)} component{?s}")
  }

  # Compute total
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

  return(result)
}


#' Validate test groups structure and column existence
#' @keywords internal
.validate_test_groups <- function(test_groups, dataf) {
  if (!is.list(test_groups) || is.null(names(test_groups))) {
    cli::cli_abort("{.arg test_groups} must be a named list of character vectors")
  }

  all_tests <- unlist(test_groups)
  missing_cols <- setdiff(all_tests, colnames(dataf))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing test columns: {paste(missing_cols, collapse = ', ')}")
  }

  return(all_tests)
}

#' Check if expected output columns already exist
#' @keywords internal
.outputs_exist <- function(dataf, expected_outputs) {
  return(all(expected_outputs %in% names(dataf)))
}

#' Get expected output columns for composite scores
#' @keywords internal
.get_expected_composite_outputs <- function(test_groups) {
  output_cols <- c()

  for (group_name in names(test_groups)) {
    # Z-score columns for this group
    zscore_cols <- paste0("zscore_", test_groups[[group_name]], "_", group_name)
    # Composite column
    composite_col <- paste0(group_name, "_comp_score")
    output_cols <- c(output_cols, zscore_cols, composite_col)
  }

  return(output_cols)
}

#' Apply filters to data frame
#' @keywords internal
.apply_filters <- function(dataf, filters) {
  if (is.null(filters)) return(dataf)

  for (filter_var in names(filters)) {
    filter_expr <- filters[[filter_var]]
    dataf <- dataf[eval(parse(text = filter_expr), dataf), ]
  }

  return(dataf)
}

#' Compute z-scores grouped by demographic variables
#' @keywords internal
.compute_grouped_zscores <- function(dataf, test_groups, grouping_vars) {
  result_df <- dataf %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars)))

  # Calculate z-scores for each test group
  for (group_name in names(test_groups)) {
    cols <- test_groups[[group_name]]
    suffix <- paste0("_", group_name)

    result_df <- result_df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols),
          ~scale(., center = TRUE, scale = TRUE)[, 1],
          .names = "zscore_{.col}{suffix}"
        )
      )
  }

  return(result_df %>% dplyr::ungroup())
}

#' Compute composite scores from z-scores
#' @keywords internal
.compute_composite_scores <- function(dataf, test_groups) {
  result_df <- dataf

  for (group_name in names(test_groups)) {
    composite_name <- paste0(group_name, "_comp_score")
    zscore_cols <- paste0("zscore_", test_groups[[group_name]], "_", group_name)

    result_df <- result_df %>%
      dplyr::mutate(
        !!composite_name := rowMeans(
          dplyr::across(dplyr::all_of(zscore_cols)),
          na.rm = TRUE
        )
      )
  }

  return(result_df)
}
