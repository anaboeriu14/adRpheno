#' Calculate BMI and classify obesity status
#'
#' Calculates BMI using imperial units (weight in pounds, height in inches),
#' renames measurement columns for clarity, and adds obesity classification.
#' Uses output-based tracking to prevent duplicate calculations.
#'
#' @param dataf A data frame containing the data
#' @param weight_col String. The name of the column containing weight in pounds (lbs)
#' @param height_col String. The name of the column containing height in inches
#' @param bmi_col String. The name of the column to store the BMI value
#' @param cutoff Numeric. BMI cutoff value for obesity classification (default: 30)
#' @param force Logical. If TRUE, recalculates even if outputs already exist (default: FALSE)
#' @param verbose Logical. If TRUE, shows informative messages (default: TRUE)
#'
#' @return Data frame with calculated BMI values and obesity classification
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate BMI and add obesity classification
#' testD <- data.frame(weight = c(150, 180, 200), height = c(65, 70, 72), BMI = c(25, 26, 10))
#' calculate_bmi_obesity(testD, "weight", "height", "BMI")
#' }
calculate_bmi_obesity <- function(dataf, weight_col, height_col, bmi_col,
                                  cutoff = 30, force = FALSE, verbose = TRUE) {

  required_cols <- c(weight_col, height_col, bmi_col)

  # Input validation
  adRutils::validate_params(
    data = dataf,
    columns = required_cols,
    numeric_columns = required_cols,
    custom_checks = list(
      list(condition = is.numeric(cutoff) && length(cutoff) ==1 && cutoff >0,
           message = "cutoff must be a single positive number"),
      list(condition = is.logical(force) | is.logical(verbose),
           message = "force & verbose must be logical (TRUE/FALSE)")
      ),
    context = "calculate_bmi_obesity"
  )

  # Define expected output columns
  expected_outputs <- c("weight_lbs", "height_inch", "bmi_imperial", "obesity")

  # Check if we should skip processing
  if (.should_skip_clinical_processing(dataf, "BMI calculation", expected_outputs,
                                       required_cols, force, verbose)) {
    return(dataf)
  }


  # Display calculation message
  if (verbose) cli_alert_info("Calculating BMI and obesity classification")

  # Rename columns for clarity and calculate BMI
  modified_df <- dataf %>%
    dplyr::rename(
      weight_lbs = !!dplyr::sym(weight_col),
      height_inch = !!dplyr::sym(height_col),
      bmi_original = !!dplyr::sym(bmi_col)
    ) %>%
    dplyr::mutate(
      bmi_imperial = round(703 * (.data$weight_lbs / .data$height_inch^2), 2),
      obesity = dplyr::if_else(.data$bmi_imperial < cutoff, 0, 1)
    )

  # Complete processing
  .complete_processing("calculate_bmi_obesity", required_cols, verbose)

  return(modified_df)
}


#' Calculate blood pressure metrics
#'
#' Calculates average systolic and diastolic blood pressure from two measurements,
#' and optionally calculates mean arterial pressure (MAP) and/or pulse pressure.
#' Uses output-based tracking to prevent duplicate calculations.
#'
#' @param dataf A data frame containing blood pressure measurements
#' @param systolic1 String. Name of the first systolic BP measurement column
#' @param systolic2 String. Name of the second systolic BP measurement column
#' @param diastolic1 String. Name of the first diastolic BP measurement column
#' @param diastolic2 String. Name of the second diastolic BP measurement column
#' @param calculate_map Logical. If TRUE, calculates mean arterial pressure (default: FALSE)
#' @param calculate_pp Logical. If TRUE, calculates pulse pressure (default: TRUE)
#' @param decimal_places Integer specifying decimal places for rounding (default: 2)
#' @param force Logical. If TRUE, recalculates even if outputs already exist (default: FALSE)
#' @param verbose Logical. If TRUE, prints informative messages (default: TRUE)
#'
#' @details
#' Mean arterial pressure (MAP) is calculated as:
#' MAP = DBP + (SBP - DBP) / 3
#'
#' Pulse pressure (PP) is calculated as:
#' PP = SBP - DBP
#'
#' where:
#' - DBP = diastolic blood pressure
#' - SBP = systolic blood pressure
#'
#' @return Data frame with added average BP columns and selected derived metrics.
#'  New columns are named `mean_arterial_pressure` or `pulse_pressure`
#' @export
#'
#' @examples
#' \dontrun{
#' test_df <- data.frame(
#'   age = c(50, 54, 32),
#'   systolic1 = c(120, 130, 125),
#'   diastolic1 = c(80, 85, 82),
#'   systolic2 = c(130, 140, 135),
#'   diastolic2 = c(85, 90, 88)
#' )
#' # Calculate PP only
#' calculate_bp_metrics(test_df, "systolic1", "systolic2", "diastolic1", "diastolic2")
#'
#' # Calculate both MAP and pulse pressure
#' calculate_bp_metrics(test_df, "systolic1", "systolic2", "diastolic1", "diastolic2",
#'                     calculate_map = TRUE, calculate_pp = TRUE)
#'
#' # Calculate MAP only
#' calculate_bp_metrics(test_df, "systolic1", "systolic2", "diastolic1", "diastolic2",
#'                     calculate_map = TRUE, calculate_pp = FALSE)
#'}
calculate_bp_metrics <- function(dataf, systolic1, systolic2, diastolic1, diastolic2,
                                 calculate_pp = TRUE, calculate_map = FALSE,
                                 decimal_places = 2, force = FALSE, verbose = TRUE) {

  required_cols <- c(systolic1, systolic2, diastolic1, diastolic2)

  # Input validation
  adRutils::validate_params(
    data = dataf,
    columns = required_cols,
    numeric_columns = required_cols,
    custom_checks = list(
      list(condition = calculate_map || calculate_pp,
           message = "At least one calculation (MAP or pulse pressure) must be requested"),
      list(condition = is.numeric(decimal_places) && length(decimal_places) == 1 && decimal_places >= 0,
           message = "decimal_places must be a single non-negative number"),
      list(condition = is.logical(force) && is.logical(verbose),
           message = "force and verbose must be logical (TRUE/FALSE)")
    ),
    context = "calculate_bp_metrics"
  )

  # step 1) get expected outputs
  expected_outputs <- .get_expected_bp_outputs(calculate_map, calculate_pp)

  #step 2) Check if we should skip processing
  if(.should_skip_clinical_processing(dataf, "Blood pressure metrics", expected_outputs,
                                       required_cols, force, verbose)) {
    return(dataf)
  }

  # step 3 Display calculation message
  .display_bp_calculation_message(calculate_map, calculate_pp, verbose)

  # compute avg_systolic_bp, avg_diastolic_bp
  result_df <- dataf %>%
    dplyr::mutate(
      avg_systolic_bp = rowMeans(cbind(.data[[systolic1]], .data[[systolic2]]), na.rm = TRUE),
      avg_diastolic_bp = rowMeans(cbind(.data[[diastolic1]], .data[[diastolic2]]), na.rm = TRUE)
    )

  #optional calculations (MAP, PP)
  if (calculate_map) {
    result_df <- result_df %>%
      dplyr::mutate(mean_arterial_pressure = round(
        .data$avg_diastolic_bp + ((.data$avg_systolic_bp - .data$avg_diastolic_bp) / 3), decimal_places))
    if (verbose) cli_alert_info("Added column 'mean_arterial_pressure'")
  }

  # calculate pulse pressure
  if (calculate_pp) {
    result_df <- result_df %>%
      dplyr::mutate(pulse_pressure = round(.data$avg_systolic_bp - .data$avg_diastolic_bp,decimal_places))
    if (verbose) cli_alert_info("Added column 'pulse_pressure'")
  }

  .complete_processing("calculate_bp_metrics", required_cols, verbose)


  return(result_df)
}

#' Calculate estimated Glomerular Filtration Rate (eGFR)
#'
#' Calculates eGFR using the CKD-EPI formula based on serum creatinine, age,
#' and sex. Uses output-based tracking to prevent duplicate calculations.
#'
#' @param dataf A data frame containing the input data
#' @param creatinine_col String specifying the column name for serum creatinine in mg/dL
#' @param age_col String specifying the column name for age in years
#' @param sex_col String specifying the column name for sex (0 for male, 1 for female)
#' @param egfr_col Character string for the output column name (default: "eGFR")
#' @param force Logical. If TRUE, recalculates even if eGFR already exists (default: FALSE)
#' @param verbose Logical. If TRUE, shows informative messages (default: TRUE)
#'
#' @return Data frame with added eGFR column
#' @export
#'
#' @details
#' Uses the CKD-EPI (Chronic Kidney Disease Epidemiology Collaboration) equation:
#'
#' For females:
#'  \deqn{eGFR = 142 \times \min\left(\frac{SCr}{\kappa}, 1\right)^\alpha
#'   \times \max\left(\frac{SCr}{\kappa}, 1\right)^{-1.200} \times 0.9938^{\text{age}} \times 1.012}
#'
#' For Males:
#' \deqn{eGFR = 142 \times \min\left(\frac{SCr}{\kappa}, 1\right)^\alpha
#'   \times \max\left(\frac{SCr}{\kappa}, 1\right)^{-1.200} \times 0.9938^{\text{age}} \times 1}
#'
#' where:
#' \itemize{
#'  \item SCr = standardized serum creatinine in mg/dL
#'   \item \eqn{\kappa} is 0.7 for females and 0.9 for males
#'   \item \eqn{\alpha} is -0.241 for females and -0.302 for males
#' }
#'
#' @examples
#' \dontrun{
#' testD <- data.frame(creatinine = c(1.2, 0.8), age = c(50, 60), sex = c(0, 1))
#' calculate_egfr(testD, "creatinine", "age", "sex")
#' }
#'
calculate_egfr <- function(dataf, creatinine_col, age_col, sex_col,
                           egfr_col = "eGFR", force = FALSE, verbose = TRUE) {

  required_cols <- c(creatinine_col, age_col, sex_col)

  # Input validation
  adRutils::validate_params(
    data = dataf,
    columns = c(creatinine_col, age_col, sex_col),
    numeric_columns = c(creatinine_col, age_col),
    custom_checks = list(
      list(condition = is.logical(force) && is.logical(verbose),
           message = "force and verbose must be logical (TRUE/FALSE)"),

      list(condition = is.character(sex_col) && length(sex_col) > 0,
           message = "sex_col must be a non-empty character vector")
    ),
    context = "calculate_egfr"
  )

  expected_outputs <- c(egfr_col)

  if (.should_skip_clinical_processing(dataf, "eGFR calculation", expected_outputs,
                                       required_cols, force, verbose)) {
    return(dataf)
  }

  if (verbose) cli_alert_info("Calculating eGFR using CKD-EPI 2021 formula")

  # Constants based on gender
  kappa <- ifelse(dataf[[sex_col]] == 1, 0.7, 0.9)        # kF for females, kM for males
  alpha <- ifelse(dataf[[sex_col]] == 1, -0.241, -0.302)  # aF for females, aM for males
  sex_factor <- ifelse(dataf[[sex_col]] == 1, 1.012, 1)   # 1.012 for females, 1 for males

  # eGFR calculation using CKD-EPI formula
  e_gfr <- 142 *
    (pmin(dataf[[creatinine_col]] / kappa, 1))^alpha *
    (pmax(dataf[[creatinine_col]] / kappa, 1))^(-1.200) *
    (0.9938^dataf[[age_col]]) * sex_factor

  # Add eGFR column to dataframe
  dataf$eGFR <- e_gfr

 .complete_processing("calculate_egfr", required_cols, verbose)

  return(dataf)
}
