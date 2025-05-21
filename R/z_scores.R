#' Standardize Numeric Variables
#'
#' Creates standardized (z-score) versions of numeric variables.
#' Tracks processing to prevent duplicate standardization.
#'
#' @param dataf A data frame
#' @param vars Character vector. Names of numeric variables to standardize
#' @param prefix Character. Prefix for standardized variable names (default: "zscore_")
#' @param group_vars Character vector. Optional grouping variables for group-wise standardization
#' @param remove_pattern Character. Optional pattern to remove from variable names in output
#' @param force Logical. If TRUE, bypasses the check for previous processing (default: FALSE)
#'
#' @return Data frame with added standardized variable columns
#' @export
#'
#' @examples
#' \dontrun{
#' testD <- data.frame(
#'   school_id = c(1, 1, -31, 13, 14, 45, 42, 15, 58, 45),
#'   grade_univ = c("Freshman", "Sophomore", "Sophomore", "Senior", "Junior",
#'                  "Senior", "Junior", "Sophomore", "Freshman", "Senior"),
#'   age = c(18, 20, -20, 22, 21, 23, 20, 19, 17, 23),
#'   score_math = c(165, 99, 110, 80, 210, 134, 78, 81, 90, 134),
#'   score_verbal = c(114, 199, 98, 80, 156, 134, 67, 140, 85, 117))
#'
#' # Simple standardization
#' standardize_variables(testD, c("score_math", "score_verbal"))
#'
#' # Group-wise standardization by grade
#' standardize_variables(testD, c("score_math", "score_verbal"), group_vars = "grade_univ")
#' }
compute_zscores <- function(dataf, vars, prefix = "zscore_",
                                  group_vars = NULL, remove_pattern = NULL,
                                  force = FALSE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  missing_cols <- vars[!vars %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if group variables exist if specified
  if (!is.null(group_vars)) {
    missing_groups <- group_vars[!group_vars %in% names(dataf)]
    if (length(missing_groups) > 0) {
      stop("The following grouping variables do not exist in the data: ",
           paste(missing_groups, collapse = ", "))
    }
  }

  # Check if variables are numeric
  non_numeric <- character()
  for (var in vars) {
    if (!is.numeric(dataf[[var]])) {
      non_numeric <- c(non_numeric, var)
    }
  }
  if (length(non_numeric) > 0) {
    stop("The following variables are not numeric: ",
         paste(non_numeric, collapse = ", "))
  }

  # Check if these variables have already been processed
  if (!force) {
    adRutils::is_processed("standardize_variables", vars, error_if_exists = TRUE)
  }

  # Create new column names
  new_names <- vars
  if (!is.null(remove_pattern)) {
    new_names <- sapply(new_names, function(name) {
      stringr::str_remove(name, remove_pattern)
    })
  }
  new_names <- paste0(prefix, new_names)
  names(new_names) <- vars

  # Create standardized data frame
  if (is.null(group_vars)) {
    # Standard z-scoring
    for (var in vars) {
      new_var <- new_names[var]
      dataf[[new_var]] <- as.vector(scale(dataf[[var]]))
    }
    df_standardized <- dataf
  } else {
    # Group-wise z-scoring - this is more complex
    df_standardized <- dataf

    # Create an expression to perform the grouping
    group_expr <- paste0("df_standardized %>% dplyr::group_by(",
                         paste(group_vars, collapse = ", "), ")")
    grouped_df <- eval(parse(text = group_expr))

    # Process each variable individually
    for (var in vars) {
      new_var <- new_names[var]
      # Create and evaluate the expression to add the standardized variable
      mutate_expr <- paste0(
        "grouped_df <- grouped_df %>% dplyr::mutate(",
        new_var, " = as.vector(scale(", var, ")))"
      )
      eval(parse(text = mutate_expr))
    }

    # Ungroup and return
    df_standardized <- grouped_df %>% dplyr::ungroup()
  }

  # Register these variables as processed
  adRutils::register_processed("standardize_variables", vars)

  return(df_standardized)
}
