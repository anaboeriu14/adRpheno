#' Standardize Numeric Variables
#'
#' Creates standardized (z-score) versions of numeric variables.
#' Z-scores have mean = 0 and standard deviation = 1.
#'
#' @param dataf A data frame
#' @param vars Character vector of numeric variable names to standardize
#' @param prefix Prefix for standardized variable names (default: `"zscore_"`)
#' @param group_vars Optional character vector of grouping variables for
#'   group-wise standardization
#' @param verbose Show informative messages (default: `TRUE`)
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
#'   vars       = c("test_score1", "test_score2"),
#'   group_vars = c("age_group", "sex")
#' )
#' }
compute_zscores <- function(dataf, vars, prefix = "zscore_",
                            group_vars = NULL, verbose = TRUE) {

  adRutils::validate_args(
    data            = dataf,
    columns         = c(vars, group_vars),
    numeric_columns = vars,
    vars            = adRutils::is_nonempty_character(),
    prefix          = adRutils::is_string(),
    verbose         = adRutils::is_flag(),
    custom_checks   = list(
      list(condition = is.null(group_vars) || is.character(group_vars),
           message   = "{.arg group_vars} must be NULL or a character vector")
    )
  )

  if (verbose) {
    if (is.null(group_vars)) {
      cli::cli_alert_info("Standardizing {length(vars)} variable{?s}")
    } else {
      cli::cli_alert_info(
        "Standardizing {length(vars)} variable{?s} by {length(group_vars)} grouping variable{?s}"
      )
    }
  }

  result_df <- if (is.null(group_vars)) {
    .compute_simple_zscores(dataf, vars, prefix)
  } else {
    .compute_grouped_zscores(dataf, vars, group_vars, prefix)
  }

  if (verbose) {
    cli::cli_alert_success("Standardization complete ({length(vars)} variable{?s} processed)")
  }

  result_df
}


# ---- internal helpers ------------------------------------------------------

#' Compute simple z-scores (no grouping)
#' @keywords internal
#' @noRd
.compute_simple_zscores <- function(dataf, vars, prefix) {
  result_df <- dataf
  for (var in vars) {
    result_df[[paste0(prefix, var)]] <- as.vector(scale(result_df[[var]]))
  }
  result_df
}

#' Compute grouped z-scores
#' @keywords internal
#' @noRd
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
