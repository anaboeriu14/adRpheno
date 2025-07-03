#' Standardize Numeric Variables
#'
#' Creates standardized (z-score) versions of numeric variables.
#'
#' @param dataf A data frame
#' @param vars Character vector. Names of numeric variables to standardize
#' @param prefix Character. Prefix for standardized variable names (default: "zscore_")
#' @param group_vars Character vector. Optional grouping variables for group-wise standardization
#' @param force Logical. If TRUE, bypasses the check for previous processing (default: FALSE)
#'
#' @return Data frame with added standardized variable columns
#' @export
compute_zscores <- function(dataf, vars, prefix = "zscore_",
                            group_vars = NULL, force = FALSE) {

  # === STEP 1: Simple validation ===
  adRutils::validate_params(
    data = dataf,
    columns = vars,
    numeric_columns = vars,
    grouping_vars = group_vars,
    custom_checks = list(
      list(
        condition = is.character(prefix) && length(prefix) == 1,
        message = "prefix must be a single character string"
      ),
      list(
        condition = is.logical(force) && length(force) == 1,
        message = "force must be TRUE or FALSE"
      )
    ),
    context = "compute_zscores"
  )

  # === STEP 2: Check processing history ===
  if (!force) {
    adRutils::is_processed("standardize_variables", vars, error_if_exists = TRUE)
  }

  # === STEP 3: Standardize variables ===
  result_df <- dataf

  if (is.null(group_vars)) {
    # Simple standardization
    for (var in vars) {
      new_var <- paste0(prefix, var)
      result_df[[new_var]] <- as.vector(scale(result_df[[var]]))
    }
  } else {
    # Group-wise standardization
    result_df <- result_df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(vars),
                      ~ as.vector(scale(.x)),
                      .names = paste0(prefix, "{.col}"))
      ) %>%
      dplyr::ungroup()
  }

  # === STEP 4: Complete ===
  adRutils::register_processed("standardize_variables", vars)
  message("Standardization complete (", length(vars), " variables processed)")

  return(result_df)
}
