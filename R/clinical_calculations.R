#' Calculate BMI and classify obesity status
#'
#' Calculates BMI using imperial units (weight in pounds, height in inches),
#' renames measurement columns for clarity, and adds obesity classification.
#'
#' @param dataf A data frame containing the data
#' @param weight_col String. Column name containing weight in pounds (lbs)
#' @param height_col String. Column name containing height in inches
#' @param bmi_col String. Column name to store the BMI value
#' @param cutoff Numeric. BMI cutoff value for obesity classification (default: 30)
#' @param verbose Logical. If TRUE, shows informative messages (default: TRUE)
#'
#' @return Data frame with calculated BMI values and obesity classification
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(weight = c(150, 180, 200), height = c(65, 70, 72))
#' calculate_bmi_obesity(df, "weight", "height", "BMI")
#' }
calculate_bmi_obesity <- function(dataf, weight_col, height_col, bmi_col,
                                  cutoff = 30, verbose = TRUE) {

  required_cols <- c(weight_col, height_col, bmi_col)

  adRutils::validate_params(
    data = dataf,
    columns = required_cols,
    numeric_columns = required_cols,
    custom_checks = list(
      list(condition = is.numeric(cutoff) && length(cutoff) == 1 && cutoff > 0,
           message = "{.arg cutoff} must be a single positive number"),
      list(condition = is.logical(verbose),
           message = "{.arg verbose} must be logical (TRUE/FALSE)")
    ),
    context = "calculate_bmi_obesity"
  )

  # Check if already processed
  expected_outputs <- c("weight_lbs", "height_inch", "bmi_imperial", "obesity")
  if (.outputs_exist(dataf, expected_outputs)) {
    if (verbose) {
      cli::cli_alert_info("BMI calculation outputs already exist. Results may be overwritten.")
    }
  }

  if (verbose) cli::cli_alert_info("Calculating BMI and obesity classification")

  result_df <- dataf %>%
    dplyr::rename(
      weight_lbs = !!dplyr::sym(weight_col),
      height_inch = !!dplyr::sym(height_col),
      bmi_original = !!dplyr::sym(bmi_col)
    ) %>%
    dplyr::mutate(
      bmi_imperial = round(703 * (.data$weight_lbs / .data$height_inch^2), 2),
      obesity = dplyr::if_else(.data$bmi_imperial < cutoff, 0, 1)
    )

  if (verbose) cli::cli_alert_success("BMI calculation complete")

  return(result_df)
}


#' Calculate blood pressure metrics
#'
#' Calculates average systolic and diastolic blood pressure from two measurements,
#' and optionally calculates mean arterial pressure (MAP) and/or pulse pressure.
#'
#' @param dataf A data frame containing blood pressure measurements
#' @param systolic1 String. Name of the first systolic BP measurement column
#' @param systolic2 String. Name of the second systolic BP measurement column
#' @param diastolic1 String. Name of the first diastolic BP measurement column
#' @param diastolic2 String. Name of the second diastolic BP measurement column
#' @param calculate_map Logical. If TRUE, calculates mean arterial pressure (default: FALSE)
#' @param calculate_pp Logical. If TRUE, calculates pulse pressure (default: TRUE)
#' @param decimal_places Integer specifying decimal places for rounding (default: 2)
#' @param verbose Logical. If TRUE, prints informative messages (default: TRUE)
#'
#' @details
#' Mean arterial pressure (MAP) is calculated as: MAP = DBP + (SBP - DBP) / 3
#' Pulse pressure (PP) is calculated as: PP = SBP - DBP
#'
#' @return Data frame with added average BP columns and selected derived metrics
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   systolic1 = c(120, 130, 125),
#'   diastolic1 = c(80, 85, 82),
#'   systolic2 = c(130, 140, 135),
#'   diastolic2 = c(85, 90, 88)
#' )
#' calculate_bp_metrics(df, "systolic1", "systolic2", "diastolic1", "diastolic2")
#' }
calculate_bp_metrics <- function(dataf, systolic1, systolic2, diastolic1, diastolic2,
                                 calculate_pp = TRUE, calculate_map = FALSE,
                                 decimal_places = 2, verbose = TRUE) {

  required_cols <- c(systolic1, systolic2, diastolic1, diastolic2)

  adRutils::validate_params(
    data = dataf,
    columns = required_cols,
    numeric_columns = required_cols,
    custom_checks = list(
      list(condition = calculate_map || calculate_pp,
           message = "At least one calculation (MAP or pulse pressure) must be requested"),
      list(condition = is.numeric(decimal_places) && length(decimal_places) == 1 && decimal_places >= 0,
           message = "{.arg decimal_places} must be a single non-negative number"),
      list(condition = is.logical(verbose),
           message = "{.arg verbose} must be logical (TRUE/FALSE)")
    ),
    context = "calculate_bp_metrics"
  )

  # Check if already processed
  expected_outputs <- .get_expected_bp_outputs(calculate_map, calculate_pp)
  if (.outputs_exist(dataf, expected_outputs)) {
    if (verbose) {
      cli::cli_alert_info("BP calculation outputs already exist. Results may be overwritten.")
    }
  }

  if (verbose) {
    calculations <- c(
      if (calculate_map) "Mean Arterial Pressure",
      if (calculate_pp) "Pulse Pressure"
    )
    msg <- if (length(calculations) > 0) {
      paste0("Calculating average BP and ", paste(calculations, collapse = ", "))
    } else {
      "Calculating average BP"
    }
    cli::cli_alert_info(msg)
  }

  # Calculate averages
  result_df <- dataf %>%
    dplyr::mutate(
      avg_systolic_bp = rowMeans(cbind(.data[[systolic1]], .data[[systolic2]]), na.rm = TRUE),
      avg_diastolic_bp = rowMeans(cbind(.data[[diastolic1]], .data[[diastolic2]]), na.rm = TRUE)
    )

  # Optional calculations
  if (calculate_map) {
    result_df <- result_df %>%
      dplyr::mutate(mean_arterial_pressure = round(
        .data$avg_diastolic_bp + ((.data$avg_systolic_bp - .data$avg_diastolic_bp) / 3),
        decimal_places))
  }

  if (calculate_pp) {
    result_df <- result_df %>%
      dplyr::mutate(pulse_pressure = round(
        .data$avg_systolic_bp - .data$avg_diastolic_bp,
        decimal_places))
  }

  if (verbose) cli::cli_alert_success("BP calculation complete")

  return(result_df)
}


#' Calculate estimated Glomerular Filtration Rate (eGFR)
#'
#' Calculates eGFR using the CKD-EPI formula based on serum creatinine, age,
#' and sex.
#'
#' @param dataf A data frame containing the input data
#' @param creatinine_col String specifying column name for serum creatinine in mg/dL
#' @param age_col String specifying column name for age in years
#' @param sex_col String specifying column name for sex (0 for male, 1 for female)
#' @param egfr_col Character string for output column name (default: "eGFR")
#' @param verbose Logical. If TRUE, shows informative messages (default: TRUE)
#'
#' @details
#' Uses the CKD-EPI (Chronic Kidney Disease Epidemiology Collaboration) equation.
#' For females: eGFR = 142 × min(SCr/κ, 1)^α × max(SCr/κ, 1)^(-1.200) × 0.9938^age × 1.012
#' For males: eGFR = 142 × min(SCr/κ, 1)^α × max(SCr/κ, 1)^(-1.200) × 0.9938^age × 1
#' where κ is 0.7 for females and 0.9 for males; α is -0.241 for females and -0.302 for males
#'
#' @return Data frame with added eGFR column
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(creatinine = c(1.2, 0.8), age = c(50, 60), sex = c(0, 1))
#' calculate_egfr(df, "creatinine", "age", "sex")
#' }
calculate_egfr <- function(dataf, creatinine_col, age_col, sex_col,
                           egfr_col = "eGFR", verbose = TRUE) {

  required_cols <- c(creatinine_col, age_col, sex_col)

  adRutils::validate_params(
    data = dataf,
    columns = required_cols,
    numeric_columns = c(creatinine_col, age_col),
    custom_checks = list(
      list(condition = is.logical(verbose),
           message = "{.arg verbose} must be logical (TRUE/FALSE)"),
      list(condition = is.character(egfr_col) && length(egfr_col) == 1,
           message = "{.arg egfr_col} must be a single character string")
    ),
    context = "calculate_egfr"
  )

  # Check if already processed
  if (egfr_col %in% names(dataf)) {
    if (verbose) {
      cli::cli_alert_info("Column '{egfr_col}' already exists and will be overwritten.")
    }
  }

  if (verbose) cli::cli_alert_info("Calculating eGFR using CKD-EPI 2021 formula")

  # CKD-EPI constants based on sex
  kappa <- ifelse(dataf[[sex_col]] == 1, 0.7, 0.9)
  alpha <- ifelse(dataf[[sex_col]] == 1, -0.241, -0.302)
  sex_factor <- ifelse(dataf[[sex_col]] == 1, 1.012, 1)

  # Calculate eGFR
  dataf[[egfr_col]] <- 142 *
    (pmin(dataf[[creatinine_col]] / kappa, 1))^alpha *
    (pmax(dataf[[creatinine_col]] / kappa, 1))^(-1.200) *
    (0.9938^dataf[[age_col]]) * sex_factor

  if (verbose) cli::cli_alert_success("eGFR calculation complete")

  return(dataf)
}


#' Check if expected output columns already exist
#' @keywords internal
.outputs_exist <- function(dataf, expected_outputs) {
  all(expected_outputs %in% names(dataf))
}

#' Determine expected BP output columns based on user options
#' @keywords internal
.get_expected_bp_outputs <- function(calculate_map, calculate_pp) {
  base_outputs <- c("avg_systolic_bp", "avg_diastolic_bp")

  optional_outputs <- c(
    if (calculate_map) "mean_arterial_pressure",
    if (calculate_pp) "pulse_pressure"
  )

  c(base_outputs, optional_outputs)
}
