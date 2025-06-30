#' Compute Adjusted Composite Scores
#'
#' Computes z-scores for test scores adjusted for demographic factors and
#' calculates composite scores for specified test groups.
#'
#' @param dataf A data frame containing the raw test scores and demographic information.
#' @param test_groups Named list of character vectors, where each element contains
#'                  column names for a test group (e.g., list(memory = c("test1", "test2"),
#'                  executive = c("test3", "test4"))).
#' @param grouping_vars Character vector of column names to group by for z-score calculation
#'                     (e.g., c("age_group", "education_group", "language")).
#' @param filters Named list of filtering conditions (optional).
#' @param digits Integer indicating the number of decimal places for rounding (default: 3).
#' @param force Logical, whether to force recalculation if already processed (default: FALSE).
#'
#' @return A data frame with the original data, adjusted z-scores, and computed composite scores.
#' @details This function expects that demographic grouping variables already exist in the data frame.
#'  To create standardized demographic groupings, consider using the `create_flexible_groups()`
#'  function from the adRutils package first.
#'
#' @seealso \code{\link[adRutils]{bin_and_categorize_variables}}
#'
#' @examples
#' \dontrun{
#' # Define test groups
#' test_groups <- list(
#'   memory = c("LM1", "LM2", "SEVLT_imm", "SEVLT_del"),
#'   executive = c("digit_span", "trails_a", "trails_b"),
#'   verbal = c("FAS", "animal_naming")
#' )
#'
#' # Calculate composite scores with demographic adjustment
#' result <- create_adjusted_composites(
#'   dataf = my_data,
#'   test_groups = test_groups,
#'   grouping_vars = c("age_group", "edu_group", "language_group")
#' )
#' }
#' @export
create_adjusted_composites <- function(dataf, test_groups, grouping_vars,
                                       filters = NULL, digits = 3, force = FALSE) {

  # Validate test groups and get all test columns
  all_tests <- .validate_test_groups(test_groups, dataf)

  # Input validation using adRutils::validate_params
  adRutils::validate_params(
    data = dataf,
    columns = c(grouping_vars, all_tests),  # Both grouping vars and test columns must exist
    numeric_columns = all_tests,  # Test columns should be numeric
    custom_checks = list(
      list(condition = is.numeric(digits) && length(digits) == 1 && digits >= 0,
           message = "digits must be a single non-negative number"),
      list(condition = is.logical(force),
           message = "force must be logical (TRUE/FALSE)"),
      list(condition = is.null(filters) || is.list(filters),
           message = "filters must be NULL or a named list")
    ),
    context = "create_adjusted_composites"
  )

  # Define expected outputs (z-scores and composite scores for each group)
  expected_outputs <- c()
  for (group_name in names(test_groups)) {
    cols <- test_groups[[group_name]]
    # Z-score columns
    zscore_cols <- paste0("zscore_", cols, "_", group_name)
    # Composite score column
    composite_col <- paste0(group_name, "_comp_score")
    expected_outputs <- c(expected_outputs, zscore_cols, composite_col)
  }

  if (.should_skip_clinical_processing(dataf, "create_demographic_adjusted_composites", expected_outputs,
                                       all_tests, force, TRUE)) {
    return(dataf)
  }

  # Display progress message
  message("Creating demographic adjusted composite scores for ", length(test_groups), " test groups")

  # Apply filters if provided
  df <- dataf
  if (!is.null(filters)) {
    for (filter_var in names(filters)) {
      filter_expr <- filters[[filter_var]]
      df <- df[eval(parse(text = filter_expr), df), ]
    }
  }

  # Create grouping specification
  grp_expr <- paste0("df %>% dplyr::group_by(",
                     paste(grouping_vars, collapse = ", "), ")")
  grouped_df <- eval(parse(text = grp_expr))

  # Calculate z-scores for each test group
  result_df <- grouped_df

  # For each test group, calculate z-scores with appropriate suffix
  for (group_name in names(test_groups)) {
    cols <- test_groups[[group_name]]
    suffix <- paste0("_", group_name)

    mutate_expr <- paste0(
      "result_df <- result_df %>% dplyr::mutate(dplyr::across(",
      "all_of(c(", paste0("'", cols, "'", collapse = ", "), ")), ",
      "~scale(., center = TRUE, scale = TRUE)[,1], ",
      ".names = 'zscore_{.col}", suffix, "'))"
    )
    eval(parse(text = mutate_expr))
  }

  # Ungroup
  result_df <- result_df %>% dplyr::ungroup()

  # Create composite scores for each test group
  for (group_name in names(test_groups)) {
    composite_name <- paste0(group_name, "_comp_score")
    suffix <- paste0("_", group_name)

    mutate_expr <- paste0(
      "result_df <- result_df %>% dplyr::mutate(",
      composite_name, " = rowMeans(dplyr::select(result_df, ",
      "dplyr::starts_with('zscore_') & dplyr::ends_with('", suffix, "')), ",
      "na.rm = TRUE))"
    )
    eval(parse(text = mutate_expr))
  }

  # Round numeric columns
  result_df <- result_df %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., digits = digits)))

  # Complete processing using shared helper
  .complete_processing("create_demographic_adjusted_composites", all_tests, TRUE)

  return(result_df)
}

#' Calculate Total Score from Multiple Test Components
#'
#' Creates a total score by combining multiple test component scores.
#' Allows for different combination methods (sum, mean, etc.).
#'
#' @param dataf A data frame containing the test component columns.
#' @param component_cols Character vector of column names to combine.
#' @param result_col Name for the new total score column (default: "total_score").
#' @param method Function to use for combining scores: "sum" or "mean" (default: "sum").
#' @param na.rm Logical, whether to remove NA values when combining (default: TRUE).
#' @param force Logical, whether to force recalculation if already processed (default: FALSE).
#'
#' @return A data frame with the original data plus a new column for the total score.
#' @examples
#' \dontrun{
#' # Sum Trail A and B times
#' result <- calculate_total_score(
#'   dataf = my_data,
#'   component_cols = c("trails_a_time", "trails_b_time"),
#'   result_col = "trails_ab_total_time"
#' )
#'
#' # Calculate mean of memory tests
#' result <- calculate_total_score(
#'   dataf = my_data,
#'   component_cols = c("recall_1", "recall_2", "recognition"),
#'   result_col = "memory_mean",
#'   method = "mean"
#' )
#' }
#' @export
sum_cognitive_test_components <- function(dataf, component_cols, result_col = "total_score",
                                          method = "sum", na.rm = TRUE, force = FALSE) {

  # Input validation using adRutils::validate_params
  adRutils::validate_params(
    data = dataf,
    columns = component_cols,  # Component columns must exist
    numeric_columns = component_cols,  # Component columns must be numeric
    custom_checks = list(
      list(condition = method %in% c("sum", "mean"),
           message = "method must be one of: sum, mean"),
      list(condition = is.logical(na.rm),
           message = "na.rm must be logical (TRUE/FALSE)"),
      list(condition = is.logical(force),
           message = "force must be logical (TRUE/FALSE)"),
      list(condition = is.character(result_col) && length(result_col) == 1,
           message = "result_col must be a single character string")
    ),
    context = "sum_cognitive_test_components"
  )

  # Define expected outputs
  expected_outputs <- c(result_col)

  # Check if we should skip processing using shared helper
  if (.should_skip_clinical_processing(dataf, "sum_cognitive_test_components", expected_outputs,
                                       c(component_cols, result_col), force, TRUE)) {
    return(dataf)
  }

  # Display progress message
  message("Creating ", result_col, " using ", method, " of ", length(component_cols), " components")

  # Create the aggregate column
  result <- dataf
  if (method == "sum") {
    result <- result %>%
      dplyr::mutate(
        !!dplyr::sym(result_col) := rowSums(
          dplyr::across(dplyr::all_of(component_cols)),
          na.rm = na.rm
        )
      )
  } else if (method == "mean") {
    result <- result %>%
      dplyr::mutate(
        !!dplyr::sym(result_col) := rowMeans(
          dplyr::across(dplyr::all_of(component_cols)),
          na.rm = na.rm
        )
      )
  }

  # Complete processing using shared helper
  .complete_processing("sum_cognitive_test_components", c(component_cols, result_col), TRUE)

  return(result)
}
