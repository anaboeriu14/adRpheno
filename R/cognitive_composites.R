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
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }
  if (!is.list(test_groups) || is.null(names(test_groups))) {
    stop("'test_groups' must be a named list of character vectors.")
  }
  if (!all(grouping_vars %in% colnames(dataf))) {
    missing <- setdiff(grouping_vars, colnames(dataf))
    stop("These grouping variables are missing from the data: ",
         paste(missing, collapse = ", "))
  }

  # Combine all test columns
  all_tests <- unlist(test_groups)

  # Check if test columns exist
  missing_cols <- setdiff(all_tests, colnames(dataf))
  if (length(missing_cols) > 0) {
    stop("These test columns are missing from the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if these columns have already been processed
  if (!force) {
    adRutils::is_processed("create_demographic_adjusted_composites", all_tests, error_if_exists = TRUE)
  }

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

  # Register these columns as processed
  adRutils::register_processed("create_demographic_adjusted_composites", all_tests)

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
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  missing_cols <- component_cols[!component_cols %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate method
  valid_methods <- c("sum", "mean")
  if (!method %in% valid_methods) {
    stop("Method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Check column types
  non_numeric <- character()
  for (col in component_cols) {
    if (!is.numeric(dataf[[col]])) {
      non_numeric <- c(non_numeric, col)
    }
  }
  if (length(non_numeric) > 0) {
    stop("The following columns are not numeric: ", paste(non_numeric, collapse = ", "))
  }

  # Check if these columns have already been processed
  if (!force) {
    adRutils::is_processed("sum_cognitive_test_components",
                            c(component_cols, result_col),
                            error_if_exists = TRUE)
  }

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

  # Register these columns as processed
  adRutils::register_processed("sum_cognitive_test_components",
                                c(component_cols, result_col))

  return(result)
}
