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
#' @param warn Logical. If TRUE, shows informative messages (default: TRUE)
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
                                  cutoff = 30, force = FALSE, warn = TRUE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  required_cols <- c(weight_col, height_col, bmi_col)
  missing_cols <- required_cols[!required_cols %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if columns are numeric
  for (col in c(weight_col, height_col)) {
    if (!is.numeric(dataf[[col]])) {
      stop("Column '", col, "' must be numeric.")
    }
  }

  # Define expected output columns
  output_cols <- c("weight_lbs", "height_inch", "bmi_imperial", "obesity")

  # Check if BMI calculation already completed (output-based check)
  if (all(output_cols %in% names(dataf))) {
    if (force) {
      if (warn) message("Recalculating BMI and obesity status (force = TRUE)")
    } else {
      if (warn) message("BMI calculation already completed. Use force=TRUE to recalculate.")
      return(dataf)
    }
  }

  # Function-specific processing check (only if not forcing)
  if (!force) {
    tryCatch({
      adRutils::is_processed("calculate_bmi_obesity", required_cols,
                             error_if_exists = TRUE)
    }, error = function(e) {
      if (grepl("already been processed", e$message)) {
        if (warn) message("BMI processing markers detected. Use force=TRUE to override.")
        return(dataf)
      } else {
        stop(e)
      }
    })
  }

  # Rename columns for clarity
  modified_df <- dataf %>%
    dplyr::rename(
      weight_lbs = !!dplyr::sym(weight_col),
      height_inch = !!dplyr::sym(height_col),
      bmi_imperial = !!dplyr::sym(bmi_col)
    )

  # Calculate BMI and obesity status
  modified_df <- modified_df %>%
    dplyr::mutate(
      bmi_imperial = round(703 * (.data$weight_lbs / .data$height_inch^2), 2),
      obesity = dplyr::if_else(.data$bmi_imperial < cutoff, 0, 1)
    )

  # Register processing (function-specific)
  adRutils::register_processed("calculate_bmi_obesity", required_cols)

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
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  required_cols <- c(systolic1, systolic2, diastolic1, diastolic2)
  missing_cols <- required_cols[!required_cols %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if columns are numeric
  for (col in required_cols) {
    if (!is.numeric(dataf[[col]])) {
      stop("Column '", col, "' must be numeric.")
    }
  }

  # Check if at least one calculation is requested
  if (!calculate_map && !calculate_pp) {
    stop("At least one calculation (MAP or pulse pressure) must be requested.")
  }

  # Define expected output columns based on what's requested
  base_output_cols <- c("avg_systolic_bp", "avg_diastolic_bp")
  optional_output_cols <- c()
  if (calculate_map) optional_output_cols <- c(optional_output_cols, "mean_arterial_pressure")
  if (calculate_pp) optional_output_cols <- c(optional_output_cols, "pulse_pressure")

  expected_outputs <- c(base_output_cols, optional_output_cols)

  # Check if BP calculations already completed (output-based check)
  if (all(expected_outputs %in% names(dataf))) {
    if (force) {
      if (verbose) message("Recalculating blood pressure metrics (force = TRUE)")
    } else {
      if (verbose) message("Blood pressure calculations already completed. Use force=TRUE to recalculate.")
      return(dataf)
    }
  }

  # Function-specific processing check (only if not forcing)
  if (!force) {
    tryCatch({
      adRutils::is_processed("calculate_bp_metrics", required_cols,
                             error_if_exists = TRUE)
    }, error = function(e) {
      if (grepl("already been processed", e$message)) {
        if (verbose) message("BP processing markers detected. Use force=TRUE to override.")
        return(dataf)
      } else {
        stop(e)
      }
    })
  }

  # Display informative message about which calculations will be performed
  if (verbose) {
    if (calculate_map && calculate_pp) {
      message("Calculating average BP values, Mean Arterial Pressure (MAP), and Pulse Pressure (PP)")
    } else if (calculate_map) {
      message("Calculating average BP values and Mean Arterial Pressure (MAP)")
    } else if (calculate_pp) {
      message("Calculating average BP values and Pulse Pressure (PP)")
    }
  }

  # Make a copy of the data frame
  result_df <- dataf

  # Calculate average systolic BP
  result_df$avg_systolic_bp <- rowMeans(
    result_df[, c(systolic1, systolic2), drop = FALSE],
    na.rm = TRUE
  )

  # Calculate average diastolic BP
  result_df$avg_diastolic_bp <- rowMeans(
    result_df[, c(diastolic1, diastolic2), drop = FALSE],
    na.rm = TRUE
  )

  # Calculate mean arterial pressure (MAP) if requested
  if (calculate_map) {
    result_df$mean_arterial_pressure <- round(
      result_df$avg_diastolic_bp +
        ((result_df$avg_systolic_bp - result_df$avg_diastolic_bp) / 3),
      decimal_places
    )
    if (verbose) message("- Added column 'mean_arterial_pressure'")
  }

  # Calculate pulse pressure if requested
  if (calculate_pp) {
    result_df$pulse_pressure <- round(
      result_df$avg_systolic_bp - result_df$avg_diastolic_bp,
      decimal_places
    )
    if (verbose) message("- Added column 'pulse_pressure'")
  }

  # Register processing (function-specific)
  adRutils::register_processed("calculate_bp_metrics", required_cols)

  if (verbose) message("BP metrics calculation complete")

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
#' @param force Logical. If TRUE, recalculates even if eGFR already exists (default: FALSE)
#' @param warn Logical. If TRUE, shows informative messages (default: TRUE)
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
calculate_egfr <- function(dataf, creatinine_col, age_col, sex_col,
                           force = FALSE, warn = TRUE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Check if columns exist
  required_cols <- c(creatinine_col, age_col, sex_col)
  missing_cols <- required_cols[!required_cols %in% names(dataf)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check column types
  if (!is.numeric(dataf[[creatinine_col]]) || !is.numeric(dataf[[age_col]])) {
    stop("Creatinine and age columns must be numeric.")
  }
  if (!all(dataf[[sex_col]] %in% c(0, 1))) {
    stop("Sex column must contain values 0 (male) or 1 (female).")
  }

  # Check if eGFR already calculated (output-based check)
  if ("eGFR" %in% names(dataf)) {
    if (force) {
      if (warn) message("Recalculating eGFR (force = TRUE)")
    } else {
      if (warn) message("eGFR already exists. Use force=TRUE to recalculate.")
      return(dataf)
    }
  }

  # Constants based on gender
  kappa <- ifelse(dataf[[sex_col]] == 1, 0.7, 0.9)        # kF for females, kM for males
  alpha <- ifelse(dataf[[sex_col]] == 1, -0.241, -0.302)  # aF for females, aM for males
  sex_factor <- ifelse(dataf[[sex_col]] == 1, 1.012, 1)   # 1.012 for females, 1 for males

  # eGFR calculation using CKD-EPI formula
  e_gfr <- 142 *
    (pmin(dataf[[creatinine_col]] / kappa, 1))^alpha *
    (pmax(dataf[[creatinine_col]] / kappa, 1))^(-1.200) *
    (0.9938^dataf[[age_col]]) *
    sex_factor

  # Add eGFR column to dataframe
  dataf$eGFR <- e_gfr

  # Register processing (function-specific)
  adRutils::register_processed("calculate_egfr", required_cols)

  if (warn) message("eGFR calculation completed successfully")

  return(dataf)
}
