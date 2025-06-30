# Internal helper to determine expected BP output columns based on user options
#' @keywords internal
.get_expected_bp_outputs <- function(calculate_map, calculate_pp){
  base_output_cols <- c("avg_systolic_bp", "avg_diastolic_bp")

  optional_output_cols <- c(
  if (calculate_map) "mean_arterial_pressure",
  if (calculate_pp)  "pulse_pressure"
  )
  c(base_output_cols, optional_output_cols)
}

# Internal helper to check if BP processing should be skipped
#' @keywords internal
.should_skip_clinical_processing <- function(dataf, function_name, expected_outputs,
                                      required_cols, force, verbose = TRUE){

  # Check if clinical calculations already exist (output-based check)
  if (all(expected_outputs %in% names(dataf)) && !force) {
    if (verbose) message(function_name, " already completed. Use force=TRUE to recalculate.")
    return(TRUE)
  }

  # Function-specific processing check (only if not forcing)
  already_processed <- !force && adRutils::is_processed(function_name, required_cols, error_if_exists = FALSE)
  if (already_processed) {
    if (verbose) message(function_name, " processing markers detected. Use force=TRUE to override.")
    return(TRUE)
  }

   if (all(expected_outputs %in% names(dataf)) && force && verbose) {
    message("Recalculating ",function_name, " (force = TRUE)")
   }
  return(FALSE)
}

# Helper function to display calculation message
#' @keywords internal
.display_bp_calculation_message <- function(calculate_map, calculate_pp, verbose) {
  if (!verbose) return()

  calculations <- c(
    if (calculate_map) "Mean Arterial Pressure (MAP)",
    if (calculate_pp) "Pulse Pressure (PP)"
  )

  message(paste0("Calculating average BP values",
                 if(length(calculations) > 0) " and ",
                 paste(calculations, collapse = ", ")))
}


# Register processed & display clinical calculation is complete message
#' @keywords internal
.complete_processing <- function(function_name, required_cols, verbose = TRUE) {
  adRutils::register_processed(function_name, required_cols)
  if (verbose) message(function_name, " calculation complete")
}

# Helper function for test groups validation
#' @keywords internal
.validate_test_groups <- function(test_groups, dataf) {
  if (!is.list(test_groups) || is.null(names(test_groups))) {
    stop("'test_groups' must be a named list of character vectors.")
  }

  # Combine all test columns and check they exist
  all_tests <- unlist(test_groups)
  missing_cols <- setdiff(all_tests, colnames(dataf))
  if (length(missing_cols) > 0) {
    stop("These test columns are missing from the data: ",
         paste(missing_cols, collapse = ", "))
  }

  return(all_tests)
}

