#' Validate APOE e4 carrier status
#'
#' Function uses these constants:
#' VALID_APOE_GENOTYPES <- c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4")
#' E4_CARRIER_GENOTYPES <- c("E2E4", "E3E4", "E4E4")
#'
#' @param dataf A data frame containing APOE genotypes and e4 carrier status
#' @param genotype The name of the APOE genotype column
#' @param e4_positive The name of the e4 carrier status column
#' @param return_all Logical, if TRUE returns the entire dataset with a validation column (default: FALSE)
#' @param true_values Vector of values that represent e4 positive status (default: c(1, "1", "yes", "y", "true", "t"))
#' @param false_values Vector of values that represent e4 negative status (default: c(0, "0", "no", "n", "false", "f"))
#' @param genotype_format String indicating the format of the genotype column. Options: "standard" (default: E2E3),
#'                       "numeric" (e.g., 23, 34), or "slash" (e.g., e2/e3)
#'
#' @return If mismatches found and return_all=FALSE: returns a data frame of mismatched records
#'         If mismatches found and return_all=TRUE: returns the entire data frame with added 'standardized_genotype'
#'         and 'e4_status_valid' columns
#'         If no mismatches found: returns message "No mismatches found"
#' @export
validate_apoe_e4_status <- function(dataf, genotype, e4_positive, return_all = FALSE,
                                    true_values = c(1, "1", "yes", "y", "true", "t"),
                                    false_values = c(0, "0", "no", "n", "false", "f"),
                                    genotype_format = c("standard", "numeric", "slash")) {

  # Match and validate format parameter
  genotype_format <- match.arg(genotype_format)

  # Input validation using adRutils
  adRutils::validate_params(
    data = dataf,
    columns = c(genotype, e4_positive),
    method = genotype_format,
    valid_methods = c("standard", "numeric", "slash"),
    custom_checks = list(
      list(
        condition = is.logical(return_all) && length(return_all) == 1,
        message = "return_all must be a single logical value"
      ),
      list(
        condition = is.vector(true_values) && length(true_values) > 0,
        message = "true_values must be a non-empty vector"
      ),
      list(
        condition = is.vector(false_values) && length(false_values) > 0,
        message = "false_values must be a non-empty vector"
      )
    ),
    context = "validate_apoe_e4_status"
  )

  # Create a working copy of the dataframe
  valid_df <- dataf

  # Standardize the genotype based on format (handles case-insensitivity)
  valid_df$standardized_genotype <- .format_apoe_genotype(dataf[[genotype]], genotype_format)

  # Function-specific validation for APOE genotypes (case-insensitive)
  invalid_genotypes <- valid_df$standardized_genotype[
    !is.na(valid_df$standardized_genotype) &
      !valid_df$standardized_genotype %in% VALID_APOE_GENOTYPES
  ]

  if (length(invalid_genotypes) > 0) {
    stop("In validate_apoe_e4_status(): Invalid APOE genotypes found: ",
         paste(unique(invalid_genotypes), collapse = ", "), call. = FALSE)
  }

  # Convert e4_positive values to standardized TRUE/FALSE
  valid_df$is_e4_positive <- .parse_e4_carrier_status(dataf[[e4_positive]], true_values, false_values)

  # Find NA values from invalid conversions
  invalid_e4_values <- which(is.na(valid_df$is_e4_positive) & !is.na(dataf[[e4_positive]]))
  if (length(invalid_e4_values) > 0) {
    warning("Found ", length(invalid_e4_values), " values in column '", e4_positive,
            "' that couldn't be classified as e4 positive or negative")
  }

  # Determine which rows should be e4 positive based on genotype
  valid_df$should_be_e4_pos <- valid_df$standardized_genotype %in% E4_CARRIER_GENOTYPES

  # Compare actual vs expected e4 status
  valid_df$e4_status_valid <- valid_df$is_e4_positive == valid_df$should_be_e4_pos

  # Find mismatches (excluding rows with NA values)
  mismatched_rows <- which(!valid_df$e4_status_valid & !is.na(valid_df$e4_status_valid))

  # Clean up temporary columns
  valid_df$is_e4_positive <- NULL
  valid_df$should_be_e4_pos <- NULL

  # Return results based on parameters
  if (length(mismatched_rows) > 0) {
    if (return_all) {
      return(valid_df)
    } else {
      return(valid_df[mismatched_rows, ])
    }
  } else {
    if (return_all) {
      return(valid_df)
    } else {
      return("No mismatches found")
    }
  }
}

#' Match APOE SNPs to genotypes
#'
#' @param dataf A data frame containing SNPs and genotypes as columns
#' @param rs7412_col The name of the rs7412 SNP column
#' @param rs429358_col The name of the rs429358 SNP column
#' @param genotype_col The name of the genotype column to check
#' @param return_all Logical, whether to return the entire dataset (default) or only mismatches
#' @param show_details Logical, whether to include detailed columns in output (default: TRUE)
#' @param genotype_format String indicating the format of the genotype column (default: "standard")
#'
#' @return A data frame with validation results
#' @export
match_snp_genotype <- function(dataf, rs7412_col, rs429358_col, genotype_col,
                               return_all = TRUE, show_details = TRUE,
                               genotype_format = c("standard", "numeric", "slash")) {

  # Match and validate format parameter
  genotype_format <- match.arg(genotype_format)

  # Input validation using adRutils
  adRutils::validate_params(
    data = dataf,
    columns = c(rs7412_col, rs429358_col, genotype_col),
    method = genotype_format,
    valid_methods = c("standard", "numeric", "slash"),
    custom_checks = list(
      list(
        condition = is.logical(return_all) && length(return_all) == 1,
        message = "return_all must be a single logical value"
      ),
      list(
        condition = is.logical(show_details) && length(show_details) == 1,
        message = "show_details must be a single logical value"
      )
    ),
    context = "match_snp_genotype"
  )

  # Create a working copy
  result_df <- dataf

  # Standardize the input genotype column
  result_df$input_genotype_std <- .format_apoe_genotype(dataf[[genotype_col]], genotype_format)

  # Function-specific validation for SNP values
  valid_snp_values <- c("TT", "CT", "TC", "CC", NA)

  # Check rs7412 values
  invalid_rs7412 <- unique(toupper(as.character(dataf[[rs7412_col]])))
  invalid_rs7412 <- invalid_rs7412[!invalid_rs7412 %in% valid_snp_values & !is.na(invalid_rs7412)]
  if (length(invalid_rs7412) > 0) {
    stop("In match_snp_genotype(): Invalid rs7412 values found: ",
         paste(invalid_rs7412, collapse = ", "), call. = FALSE)
  }

  # Check rs429358 values
  invalid_rs429358 <- unique(toupper(as.character(dataf[[rs429358_col]])))
  invalid_rs429358 <- invalid_rs429358[!invalid_rs429358 %in% valid_snp_values & !is.na(invalid_rs429358)]
  if (length(invalid_rs429358) > 0) {
    stop("In match_snp_genotype(): Invalid rs429358 values found: ",
         paste(invalid_rs429358, collapse = ", "), call. = FALSE)
  }

  # Standardize SNP columns (handle both CT and TC as equivalent)
  result_df$rs7412_std <- .clean_snp_genotype_values(dataf[[rs7412_col]])
  result_df$rs429358_std <- .clean_snp_genotype_values(dataf[[rs429358_col]])

  # Create lookup table for SNP combinations to expected genotypes
  snp_lookup <- .create_snp_to_apoe_lookup()

  # Add SNP-predicted genotype
  result_df$snp_derived_genotype <- .predict_apoe_from_snps(
    result_df$rs7412_std,
    result_df$rs429358_std,
    snp_lookup
  )

  # Check if input genotype matches SNP-predicted genotype
  result_df$genotype_match <- ifelse(
    !is.na(result_df$snp_derived_genotype) &
      !is.na(result_df$input_genotype_std) &
      result_df$input_genotype_std == result_df$snp_derived_genotype,
    "yes",
    "no"
  )

  # Set NA for rows with missing data
  na_rows <- is.na(result_df$rs7412_std) |
    is.na(result_df$rs429358_std) |
    is.na(result_df$input_genotype_std)
  result_df$genotype_match[na_rows] <- NA

  # Clean up temporary SNP columns
  result_df$rs7412_std <- NULL
  result_df$rs429358_std <- NULL

  # Handle output columns based on show_details
  if (!show_details) {
    result_df$input_genotype_std <- NULL
    result_df$snp_derived_genotype <- NULL
  }

  # Return based on return_all parameter
  if (!return_all) {
    mismatched_rows <- which(result_df$genotype_match == "no")
    if (length(mismatched_rows) > 0) {
      return(result_df[mismatched_rows, ])
    } else {
      return("No mismatches found")
    }
  } else {
    return(result_df)
  }
}

#' Classify APOE genotypes into risk groups
#'
#' @param dataf A data frame containing APOE genotype data
#' @param genotype_col A character string with the name of the genotype column
#' @param group_col The name to use for the new risk group column (default: "apoe_risk_group")
#' @param na.rm Logical, whether to ignore NA values (default: FALSE)
#' @param error_on_invalid Logical, whether to error on invalid genotypes (default: TRUE)
#' @param genotype_format String indicating the format of the genotype column (default: "standard")
#'
#' @return A data frame with an additional factor column containing the APOE risk groups
#' @export
classify_apoe_risk_groups <- function(dataf, genotype_col, group_col = "apoe_risk_group",
                                      na.rm = FALSE, error_on_invalid = TRUE,
                                      genotype_format = c("standard", "numeric", "slash")) {

  # Match and validate format parameter
  genotype_format <- match.arg(genotype_format)

  # Input validation using adRutils
  adRutils::validate_params(
    data = dataf,
    columns = genotype_col,
    method = genotype_format,
    valid_methods = c("standard", "numeric", "slash"),
    custom_checks = list(
      list(
        condition = is.character(group_col) && length(group_col) == 1 && nchar(group_col) > 0,
        message = "group_col must be a single non-empty character string"
      ),
      list(
        condition = is.logical(na.rm) && length(na.rm) == 1,
        message = "na.rm must be a single logical value"
      ),
      list(
        condition = is.logical(error_on_invalid) && length(error_on_invalid) == 1,
        message = "error_on_invalid must be a single logical value"
      ),
      list(
        condition = !group_col %in% names(dataf),
        message = paste("Column name '", group_col, "' already exists in the data frame")
      )
    ),
    context = "classify_apoe_risk_groups"
  )

  # Create a working copy
  result_df <- dataf

  # Standardize genotype based on format
  result_df$std_genotype <- .format_apoe_genotype(dataf[[genotype_col]], genotype_format)

  # Define valid APOE genotypes (always uppercase for consistency)
  VALID_APOE_GENOTYPES <- c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4")

  # Check for invalid genotypes (excluding NAs) - case-insensitive
  non_na_genotypes <- result_df$std_genotype[!is.na(result_df$std_genotype)]
  invalid_genotypes <- setdiff(non_na_genotypes, VALID_APOE_GENOTYPES)

  # Handle invalid genotypes based on user preference
  if (length(invalid_genotypes) > 0) {
    if (error_on_invalid) {
      stop("In classify_apoe_risk_groups(): Invalid APOE genotypes found: ",
           paste(invalid_genotypes, collapse = ", "), call. = FALSE)
    } else {
      warning("Invalid APOE genotypes will be set to NA: ",
              paste(invalid_genotypes, collapse = ", "))
    }
  }

  # Create risk groups using a cleaner approach
  result_df[[group_col]] <- .classify_apoe_to_risk_group(result_df$std_genotype)

  # Convert to factor with proper levels for statistical contrasts
  result_df[[group_col]] <- factor(result_df[[group_col]],
                                   levels = c("e3/e3", "e2+", "e4+"))

  # Remove temporary column
  result_df$std_genotype <- NULL

  return(result_df)
}
