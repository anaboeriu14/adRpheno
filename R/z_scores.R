#' Standardize Numeric Variables
#'
#' Creates standardized (z-score) versions of numeric variables.
#' Z-scores have mean = 0 and standard deviation = 1.
#'
#' @param dataf A data frame
#' @param vars Character vector of numeric variable names to standardize
#' @param prefix Prefix for standardized variable names (default: "zscore_")
#' @param group_vars Character vector of grouping variables for group-wise
#'   standardization (optional)
#' @param verbose Show informative messages (default: TRUE)
#'
#' @return Data frame with added standardized variable columns
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple standardization
#' result <- compute_zscores(df, vars = c("age", "weight", "height"))
#'
#' # Group-wise standardization
#' result <- compute_zscores(
#'   df,
#'   vars = c("test_score1", "test_score2"),
#'   group_vars = c("age_group", "sex")
#' )
#' }
compute_zscores <- function(dataf, vars, prefix = "zscore_",
                            group_vars = NULL, verbose = TRUE) {

  adRutils::validate_params(
    data = dataf,
    columns = vars,
    numeric_columns = vars,
    grouping_vars = group_vars,
    custom_checks = list(
      list(
        condition = is.character(prefix) && length(prefix) == 1,
        message = "{.arg prefix} must be a single character string"
      ),
      list(
        condition = is.logical(verbose) && length(verbose) == 1,
        message = "{.arg verbose} must be TRUE or FALSE"
      )
    ),
    context = "compute_zscores"
  )

  # Check if outputs already exist
  expected_outputs <- paste0(prefix, vars)
  if (.outputs_exist(dataf, expected_outputs) && verbose) {
    cli::cli_alert_info("Z-score columns already exist. Results may be overwritten.")
  }

  # Display what we're doing
  if (verbose) {
    if (is.null(group_vars)) {
      cli::cli_alert_info("Standardizing {length(vars)} variable{?s}")
    } else {
      cli::cli_alert_info("Standardizing {length(vars)} variable{?s} by {length(group_vars)} grouping variable{?s}")
    }
  }

  # Compute z-scores
  if (is.null(group_vars)) {
    result_df <- .compute_simple_zscores(dataf, vars, prefix)
  } else {
    result_df <- .compute_grouped_zscores(dataf, vars, group_vars, prefix)
  }

  if (verbose) {
    cli::cli_alert_success("Standardization complete ({length(vars)} variable{?s} processed)")
  }

  return(result_df)
}


#' Check if expected output columns already exist
#' @keywords internal
.outputs_exist <- function(dataf, expected_outputs) {
  all(expected_outputs %in% names(dataf))
}

#' Compute simple z-scores (no grouping)
#' @keywords internal
.compute_simple_zscores <- function(dataf, vars, prefix) {
  result_df <- dataf

  for (var in vars) {
    new_var <- paste0(prefix, var)
    result_df[[new_var]] <- as.vector(scale(result_df[[var]]))
  }

  result_df
}

#' Compute grouped z-scores
#' @keywords internal
.compute_grouped_zscores <- function(dataf, vars, group_vars, prefix) {
  dataf %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars),
        ~as.vector(scale(.x)),
        .names = paste0(prefix, "{.col}")
      )
    ) %>%
    dplyr::ungroup()
}
