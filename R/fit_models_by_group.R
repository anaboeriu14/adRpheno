#' Fit Linear Models Across Outcomes and Groups
#'
#' Fits linear models for multiple outcomes across different groups (e.g., ancestry, sex,
#' treatment groups, etc.) with flexible covariate specifications.
#'  Allows for outcome-specific and group-specific covariates to be added to base predictors.
#'
#' @param data A data frame containing the variables for analysis
#' @param outcomes Character vector of outcome variable names to model
#' @param base_predictors Character vector of predictor variable names to include in all models
#' @param group_col Character string specifying the column name for grouping variable. Default is "superpop"
#' @param groups Character vector specifying which groups to analyze. Use "All" to include all data without grouping. Default is "All"
#' @param model_type Character string specifying the model type. Must be one of: "main", "interaction", "nested", or "stratified". Default is "main"
#' @param outcome_covariates Optional named list of additional covariates specific to certain outcomes.
#'   Keys should be outcome names, values should be character vectors of covariate names.
#' @param group_covariates Optional named list of additional covariates specific to certain groups.
#'   Keys should be group names, values should be character vectors of covariate names.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{outcome}{The outcome variable name}
#'   \item{superpop}{The group being analyzed}
#'   \item{model}{The model type specified}
#'   \item{predictors}{The full predictor formula string used}
#'   \item{model_equation}{The complete model equation (outcome ~ predictors)}
#'   \item{res}{List column containing the fitted lm objects}
#'   \item{dataf}{List column containing tidy model results from broom::tidy()}
#'   \item{n_obs}{Number of observations used in each model}
#' }
#'
#' @examples
#' \dontrun{
#' # Simple biomarker analysis across ancestry groups
#' bio_results <- fit_group_models(
#'   data = your_data,
#'   outcomes = c("verbal_ability", "memory"),
#'   base_predictors = c("age", "id_gender", "apoe_risk_group"),
#'   groups = c("AFR", "EUR", "AMR")
#' )
#'
#' # Biomarker analysis with outcome-specific covariates
#' bio_results <- fit_group_models(
#'   data = your_data,
#'   outcomes = c("tau", "ab42_ab40", "ptau", "nfl"),
#'   base_predictors = c("age", "id_gender", "apoe_risk_group"),
#'   groups = c("AFR", "EUR", "AMR"),
#'   outcome_covariates = list(
#'     "tau" = c("i_bmi", "eGFR"),
#'     "ab42_ab40" = c("i_bmi", "eGFR"),
#'     "ptau" = c("i_bmi", "eGFR"),
#'     "nfl" = c("i_bmi", "eGFR")
#'   )
#' )
#'
#' # Imaging analysis with group-specific adjustments
#' img_results <- fit_group_models(
#'   data = your_data,
#'   outcomes = c("meta_roi", "z_hippcampul_vol"),
#'   base_predictors = c("age", "id_gender", "icv", "apoe_risk_group"),
#'   groups = c("AFR", "EUR", "AMR"),
#'   group_covariates = list(
#'     "EUR" = "mri_scanner",
#'     "AMR" = "mri_scanner"
#'   )
#' )
#'
#' # Analysis combining both outcome and group specific covariates
#' mixed_results <- fit_group_models(
#'   data = your_data,
#'   outcomes = c("mmse_total", "cdr_sum"),
#'   base_predictors = c("age", "id_gender", "apoe_risk_group"),
#'   groups = c("AFR", "EUR", "AMR"),
#'   outcome_covariates = list(
#'     "mmse_total" = "id_education",
#'     "cdr_sum" = "id_education"
#'   ),
#'   group_covariates = list(
#'     "EUR" = "interview_language",
#'     "AMR" = "interview_language"
#'   )
#' )
#'
#' # Analysis without grouping (all data combined)
#' all_results <- fit_group_models(
#'   data = your_data,
#'   outcomes = c("tau", "ptau"),
#'   base_predictors = c("age", "id_gender", "ancestry", "apoe_risk_group")
#' )
#' }
#' @export
fit_group_models <- function(data,
                             outcomes,
                             base_predictors,
                             group_col = "superpop",
                             groups = "All",
                             model_type = "main",
                             outcome_covariates = NULL,
                             group_covariates = NULL) {

  # Get all potential covariate columns for validation
  all_potential_covs <- unique(c(unlist(outcome_covariates),
    unlist(group_covariates)
  ))

  validate_params(
    data = data,
    columns = c(group_col, outcomes, base_predictors, all_potential_covs),
    grouping_vars = group_col,
    method = model_type,
    valid_methods = c("main", "interaction", "nested", "stratified"),
    custom_checks = list(
      list(
        condition = is.character(outcomes) && length(outcomes) > 0,
        message = "outcomes must be a non-empty character vector"
      ),
      list(
        condition = is.character(base_predictors) && length(base_predictors) > 0,
        message = "base_predictors must be a non-empty character vector"
      ),
      list(
        condition = is.character(groups) && length(groups) > 0,
        message = "groups must be a non-empty character vector"
      )
    ),
    context = "fit_group_models"
  )

  combinations <- expand_grid(outcome = outcomes,group = groups)

  results <- map(1:nrow(combinations), function(i) {
    curr_outcome <- combinations$outcome[i]
    curr_group <- combinations$group[i]

    predictors <- .build_predictors(
      base_predictors,
      curr_outcome,
      curr_group,
      outcome_covariates,
      group_covariates
    )

    .fit_single_model(curr_outcome, curr_group, predictors, data, group_col, model_type)
  }) %>%
    list_rbind()

  return(results)
}

# Helper functions (not exported)
#' @keywords internal
.build_predictors <- function(base_predictors, outcome,
                              group, outcome_covariates = NULL,
                              group_covariates = NULL) {
  all_predictors <- base_predictors

  if (!is.null(outcome_covariates) && outcome %in% names(outcome_covariates)) {
    all_predictors <- c(all_predictors, outcome_covariates[[outcome]])
  }

  if (!is.null(group_covariates) && group %in% names(group_covariates)) {
    all_predictors <- c(all_predictors, group_covariates[[group]])
  }

  paste(all_predictors, collapse = " + ")
}



#' @keywords internal
.fit_single_model <- function(outcome, group, predictors,
                             data, group_col, model_type) {
  analysis_data  <- if (group == "All") {
    data
  } else {
    data %>% filter(.data[[group_col]] == group)
  }

  model_equation <-  glue("{outcome} ~ {predictors}")

  model_fit <- lm(as.formula(model_equation), data = analysis_data)

  model_res = tibble(
    outcome = outcome,
    superpop = group,
    model = model_type,
    predictors = predictors,
    model_equation = model_equation,
    res = list(model_fit),
    dataf = list(tidy(model_fit)),
    n_obs = nobs(model_fit)
  )
  return(model_res)
}
