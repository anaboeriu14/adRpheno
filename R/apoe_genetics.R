
#' Validate APOE e4 carrier status
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
#' @details
#' APOE e4 carrier status should be assigned to genotypes containing e4: e2/e4, e3/e4, e4/e4
#' All other genotypes should be e4 negative.
#'
#' The function handles multiple formats of genotype data:
#' - Standard format: E2E3, e2e4, E3E4, etc.
#' - Numeric format: 23, 34, 44, etc.
#' - Slash format: e2/e3, E3/E4, etc.
#'
#' The function also handles various representations of e4 positive/negative status:
#' - Binary: 0/1
#' - Text: yes/no, y/n, true/false, t/f (case-insensitive)
#' @export
#'
#' @examples
#' # Standard format with binary e4 status
#' test_data1 <- data.frame(
#'   subject_id = c("S001", "S002", "S003"),
#'   apoe_genotype = c("E2E4", "E3E3", "E3E4"),
#'   e4_status = c(1, 0, 1)
#' )
#' validate_apoe_e4_status(test_data1, "apoe_genotype", "e4_status")
#'
#' # Yes/No format with slash genotypes
#' test_data2 <- data.frame(
#'   subject_id = c("S001", "S002", "S003"),
#'   apoe_alleles = c("e2/e4", "e3/e3", "e3/e4"),
#'   e4_carrier = c("Yes", "No", "No")  # Note: S003 is incorrectly labeled
#' )
#' validate_apoe_e4_status(test_data2, "apoe_alleles", "e4_carrier",
#'                         genotype_format = "slash")
#'
#' # Numeric format genotypes
#' test_data3 <- data.frame(
#'   subject_id = c("S001", "S002", "S003"),
#'   apoe_numeric = c("24", "33", "34"),
#'   has_e4 = c("TRUE", "FALSE", "TRUE")
#' )
#' validate_apoe_e4_status(test_data3, "apoe_numeric", "has_e4",
#'                         genotype_format = "numeric")
validate_apoe_e4_status <- function(dataf, genotype, e4_positive, return_all = FALSE,
                                    true_values = c(1, "1", "yes", "y", "true", "t"),
                                    false_values = c(0, "0", "no", "n", "false", "f"),
                                    genotype_format = c("standard", "numeric", "slash")) {
  # Match and validate format parameter
  genotype_format <- match.arg(genotype_format)

  # Validate input parameters
  if (!is.data.frame(dataf)) stop("Input must be a data frame")
  if (!genotype %in% names(dataf)) stop(paste("Column", genotype, "not found in the data frame"))
  if (!e4_positive %in% names(dataf)) stop(paste("Column", e4_positive, "not found in the data frame"))

  # Create a working copy of the dataframe
  valid_df <- dataf

  # Standardize the genotype based on format
  valid_df$standardized_genotype <- standardize_genotype(dataf[[genotype]], genotype_format)

  # Define e4 carrier genotypes (case-insensitive)
  e4_carrier_genotypes <- c("E2E4", "E3E4", "E4E4")

  # Convert e4_positive values to standardized TRUE/FALSE
  valid_df$is_e4_positive <- standardize_e4_status(dataf[[e4_positive]], true_values, false_values)

  # Find NA values from invalid conversions
  invalid_e4_values <- which(is.na(valid_df$is_e4_positive) & !is.na(dataf[[e4_positive]]))
  if (length(invalid_e4_values) > 0) {
    warning(paste("Found", length(invalid_e4_values), "values in column", e4_positive,
                  "that couldn't be classified as e4 positive or negative"))
  }

  # Determine which rows should be e4 positive based on genotype
  valid_df$should_be_e4_pos <- valid_df$standardized_genotype %in% e4_carrier_genotypes

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
      return(valid_df)  # Return entire dataset with validation columns
    } else {
      return(valid_df[mismatched_rows, ])  # Return only mismatched rows
    }
  } else {
    if (return_all) {
      return(valid_df)  # Return entire dataset with validation columns
    } else {
      return("No mismatches found")
    }
  }
}

#' Standardize APOE genotype formats
#'
#' @param genotype_vector Vector of genotype values to standardize
#' @param format Format of the input genotypes ("standard", "numeric", or "slash")
#'
#' @return Vector of standardized genotypes in E2E3 format
#' @keywords internal
standardize_genotype <- function(genotype_vector, format = c("standard", "numeric", "slash")) {
  format <- match.arg(format)

  # Convert to character to ensure consistent handling
  genotype_char <- as.character(genotype_vector)

  # Initialize result vector
  result <- rep(NA_character_, length(genotype_char))

  # Process based on format
  for (i in seq_along(genotype_char)) {
    if (is.na(genotype_char[i])) next

    # Remove any whitespace and convert to uppercase
    clean_genotype <- toupper(gsub("\\s+", "", genotype_char[i]))

    if (format == "standard") {
      # Handle E2E3 format - just standardize case
      if (grepl("^[E]?[234][E]?[234]$", clean_genotype, ignore.case = TRUE)) {
        # Extract just the numbers
        numbers <- as.numeric(strsplit(gsub("[^234]", "", clean_genotype), "")[[1]])
        if (length(numbers) == 2) {
          # Create standardized format
          result[i] <- paste0("E", min(numbers), "E", max(numbers))
        }
      }
    } else if (format == "numeric") {
      # Handle 23, 34, etc. format
      if (grepl("^[234]{2}$", clean_genotype)) {
        # Extract digits
        digits <- as.numeric(strsplit(clean_genotype, "")[[1]])
        # Create standardized format
        result[i] <- paste0("E", min(digits), "E", max(digits))
      }
    } else if (format == "slash") {
      # Handle e2/e3, E3/E4 format
      if (grepl("[E]?[234][/-][E]?[234]", clean_genotype, ignore.case = TRUE)) {
        # Extract numbers
        numbers <- as.numeric(strsplit(gsub("[^234]", "", clean_genotype), "")[[1]])
        if (length(numbers) == 2) {
          # Create standardized format
          result[i] <- paste0("E", min(numbers), "E", max(numbers))
        }
      }
    }
  }

  return(result)
}

#' Standardize e4 carrier status values
#'
#' @param status_vector Vector of e4 status values
#' @param true_values Vector of values that represent e4 positive
#' @param false_values Vector of values that represent e4 negative
#'
#' @return Logical vector of standardized e4 status (TRUE = e4 positive)
#' @keywords internal
standardize_e4_status <- function(status_vector, true_values, false_values) {
  # Convert to character for consistent handling
  status_char <- as.character(status_vector)

  # Initialize result vector
  result <- rep(NA, length(status_char))

  # Convert true_values and false_values to lowercase for case-insensitive comparison
  true_values_lower <- tolower(as.character(true_values))
  false_values_lower <- tolower(as.character(false_values))

  # Process each value
  for (i in seq_along(status_char)) {
    if (is.na(status_char[i])) next

    # Convert to lowercase for comparison
    value_lower <- tolower(status_char[i])

    if (value_lower %in% true_values_lower) {
      result[i] <- TRUE
    } else if (value_lower %in% false_values_lower) {
      result[i] <- FALSE
    }
    # else: leave as NA for values that don't match either set
  }

  return(result)
}


#' Match APOE SNPs to genotypes
#'
#' @param dataf A data frame containing SNPs and genotypes as columns
#' @param rs7412_col The name of the rs7412 SNP column (TT=e2, CT=e3, CC=e4 determining)
#' @param rs429358_col The name of the rs429358 SNP column (TT=e2/e3, CT=e4, CC=e4 determining)
#' @param genotype_col The name of the genotype column to check
#' @param return_all Logical, whether to return the entire dataset (default) or only mismatches
#' @param show_details Logical, whether to include detailed columns in output (default: TRUE)
#' @param genotype_format String indicating the format of the genotype column (default: "standard")
#'                       Options: "standard" (E2E3), "numeric" (23), or "slash" (e2/e3)
#'
#' @return A data frame with added columns when show_details=TRUE:
#'  - `input_genotype_std`: The standardized version of the input genotype
#'  - `snp_derived_genotype`: The expected APOE genotype based on SNPs
#'  - `genotype_match`: "yes" if genotype matches SNPs, "no" if it doesn't
#'
#'  When show_details=FALSE, only the genotype_match column is added
#'  If return_all=FALSE, only returns rows with mismatches
#'
#' @details
#' This function validates whether the SNP combinations match the expected APOE genotypes.
#' APOE genotype is determined by two SNPs:
#'  - rs7412 (T/C): T allele = ε2, C allele = ε3/ε4
#'  - rs429358 (T/C): T allele = ε2/ε3, C allele = ε4
#'
#' The combinations lead to the following genotypes:
#'  - rs7412(TT) + rs429358(TT) = E2E2
#'  - rs7412(TT) + rs429358(CT) = E2E3
#'  - rs7412(CT) + rs429358(CT) = E2E4
#'  - rs7412(TT) + rs429358(CC) = E3E3
#'  - rs7412(CT) + rs429358(CC) = E3E4
#'  - rs7412(CC) + rs429358(CC) = E4E4
#'
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   id = c(1, 2, 3, 4, 5, 6),
#'   rs7412 = c("TT", "TT", "CT", "TT", "CT", "CC"),
#'   rs429358 = c("TT", "CT", "CT", "CC", "CC", "CC"),
#'   genotype = c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4")
#' )
#' # Check all genotypes
#' match_snp_genotype(test_data, "rs7412", "rs429358", "genotype")
#'
#' # Only return mismatches with minimal columns
#' test_data$genotype[3] <- "E3E3"  # Introduce a mismatch
#' match_snp_genotype(test_data, "rs7412", "rs429358", "genotype",
#'                    return_all = FALSE, show_details = FALSE)
match_snp_genotype <- function(dataf, rs7412_col, rs429358_col, genotype_col,
                               return_all = TRUE, show_details = TRUE,
                               genotype_format = c("standard", "numeric", "slash")) {
  # Match and validate format parameter
  genotype_format <- match.arg(genotype_format)

  # Input validation
  if (!is.data.frame(dataf)) stop("Input must be a data frame")

  # Check columns exist
  required_cols <- c(rs7412_col, rs429358_col, genotype_col)
  missing_cols <- setdiff(required_cols, names(dataf))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse=", ")))
  }

  # Create a working copy
  result_df <- dataf

  # Standardize the input genotype column
  if (genotype_format != "standard") {
    result_df$input_genotype_std <- standardize_genotype(dataf[[genotype_col]], genotype_format)
  } else {
    result_df$input_genotype_std <- toupper(as.character(dataf[[genotype_col]]))
  }

  # Standardize SNP columns
  result_df$rs7412_std <- toupper(as.character(dataf[[rs7412_col]]))
  result_df$rs429358_std <- toupper(as.character(dataf[[rs429358_col]]))

  # Create a lookup table for SNP combinations to expected genotypes
  snp_to_genotype <- data.frame(
    rs7412 = c("TT", "TT", "CT", "TT", "CT", "CC"),
    rs429358 = c("TT", "CT", "CT", "CC", "CC", "CC"),
    predicted_genotype = c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4"),
    stringsAsFactors = FALSE
  )

  # Add SNP-predicted genotype based on SNPs
  result_df$snp_derived_genotype <- NA_character_
  for (i in 1:nrow(result_df)) {
    if (is.na(result_df$rs7412_std[i]) || is.na(result_df$rs429358_std[i])) next

    # Find matching row in lookup table
    lookup_idx <- which(snp_to_genotype$rs7412 == result_df$rs7412_std[i] &
                          snp_to_genotype$rs429358 == result_df$rs429358_std[i])

    if (length(lookup_idx) == 1) {
      result_df$snp_derived_genotype[i] <- snp_to_genotype$predicted_genotype[lookup_idx]
    }
  }

  # Check if input genotype matches SNP-predicted genotype
  result_df$genotype_match <- ifelse(
    !is.na(result_df$snp_derived_genotype) &
      result_df$input_genotype_std == result_df$snp_derived_genotype,
    "yes",
    "no"
  )

  # For NAs in SNPs or genotype, set match to NA
  na_rows <- is.na(result_df$rs7412_std) |
    is.na(result_df$rs429358_std) |
    is.na(result_df$input_genotype_std)
  result_df$genotype_match[na_rows] <- NA

  # Clean up temporary SNP columns
  result_df$rs7412_std <- NULL
  result_df$rs429358_std <- NULL

  # If show_details is FALSE, remove the detailed columns
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
#' This function validates APOE genotypes and classifies them into risk groups
#' based on established research on APOE allele-related risk for Alzheimer's disease.
#'
#' @param dataf A data frame containing APOE genotype data
#' @param genotype_col A character string with the name of the genotype column
#' @param group_col The name to use for the new risk group column (default: "apoe_risk_group")
#' @param na.rm Logical, whether to ignore NA values (default: FALSE)
#' @param error_on_invalid Logical, whether to error on invalid genotypes (default: TRUE).
#'        If FALSE, invalid values will be assigned NA in the risk group.
#' @param genotype_format String indicating the format of the genotype column (default: "standard").
#'        Options: "standard" (E2E3), "numeric" (23), or "slash" (e2/e3)
#'
#' @details
#' The function recognizes six valid APOE genotypes (case-insensitive and in various formats):
#' - E2/E2, E2/E3: Classified as "e2+" (potentially protective)
#' - E3/E3: Classified as "e3/e3" (reference/neutral risk)
#' - E2/E4, E3/E4, E4/E4: Classified as "e4+" (increased risk)
#'
#' The risk grouping is based on research showing e4 alleles increase Alzheimer's risk,
#' while e2 alleles may be protective. The e3/e3 genotype is considered the population
#' reference.
#'
#' The resulting risk group column is a factor with levels ordered as "e3/e3" (reference),
#' "e2+", and "e4+" to facilitate statistical contrasts.
#'
#' @return A data frame with an additional factor column containing the APOE risk groups.
#'
#' @examples
#' # Basic usage with standard format genotypes
#' test_data <- data.frame(
#'   id = 1:7,
#'   apoe = c("E2E2", "E2E3", "E3E3", "E2E4", "E3E4", "E4E4", NA)
#' )
#' result <- classify_apoe_risk_groups(test_data, "apoe")
#'
#' # Using alternative formats
#' numeric_data <- data.frame(
#'   id = 1:3,
#'   apoe_numeric = c("22", "23", "34")
#' )
#' result2 <- classify_apoe_risk_groups(numeric_data, "apoe_numeric",
#'                                      genotype_format = "numeric")
#'
#' # Custom risk group column name
#' result3 <- classify_apoe_risk_groups(test_data, "apoe", group_col = "ad_risk")
#'
#' # Handle invalid genotypes without error
#' invalid_data <- data.frame(
#'   id = 1:3,
#'   apoe = c("E2E2", "INVALID", "E4E4")
#' )
#' result4 <- classify_apoe_risk_groups(invalid_data, "apoe",
#'                                     error_on_invalid = FALSE)
#' @export
classify_apoe_risk_groups <- function(dataf, genotype_col, group_col = "apoe_risk_group",
                                      na.rm = FALSE, error_on_invalid = TRUE,
                                      genotype_format = c("standard", "numeric", "slash")) {
  # Match and validate format parameter
  genotype_format <- match.arg(genotype_format)

  # Validate input data
  if (!is.data.frame(dataf)) stop("Input must be a data frame")
  if (!genotype_col %in% names(dataf)) {
    stop(paste("Column", genotype_col, "does not exist in the data frame"))
  }

  # Create a working copy
  result_df <- dataf

  # Standardize genotype based on format
  if (genotype_format != "standard") {
    result_df$std_genotype <- standardize_genotype(dataf[[genotype_col]], genotype_format)
  } else {
    result_df$std_genotype <- toupper(as.character(dataf[[genotype_col]]))
  }

  # Define valid APOE genotypes
  valid_genotypes <- c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4")

  # Check for invalid genotypes (excluding NAs)
  non_na_genotypes <- result_df$std_genotype[!is.na(result_df$std_genotype)]
  invalid_genotypes <- setdiff(non_na_genotypes, valid_genotypes)

  # Handle invalid genotypes based on user preference
  if (length(invalid_genotypes) > 0) {
    if (error_on_invalid) {
      stop(paste("Invalid APOE genotypes found:",
                 paste(invalid_genotypes, collapse = ", ")))
    } else {
      warning(paste("Invalid APOE genotypes will be set to NA:",
                    paste(invalid_genotypes, collapse = ", ")))
    }
  }

  # Create risk groups
  result_df[[group_col]] <- dplyr::case_when(
    result_df$std_genotype %in% c("E2E2", "E2E3") ~ "e2+",
    result_df$std_genotype == "E3E3" ~ "e3/e3",
    result_df$std_genotype %in% c("E2E4", "E3E4", "E4E4") ~ "e4+",
    TRUE ~ NA_character_
  )

  # Convert to factor with proper levels for statistical contrasts
  result_df[[group_col]] <- factor(result_df[[group_col]],
                                   levels = c("e3/e3", "e2+", "e4+"))

  # Remove temporary column
  result_df$std_genotype <- NULL

  return(result_df)
}

#' Standardize APOE genotype formats
#'
#' @param genotype_vector Vector of genotype values to standardize
#' @param format Format of the input genotypes ("standard", "numeric", or "slash")
#'
#' @return Vector of standardized genotypes in E2E3 format
#' @keywords internal
standardize_genotype <- function(genotype_vector, format = c("standard", "numeric", "slash")) {
  format <- match.arg(format)

  # Convert to character to ensure consistent handling
  genotype_char <- as.character(genotype_vector)

  # Initialize result vector
  result <- rep(NA_character_, length(genotype_char))

  # Process based on format
  for (i in seq_along(genotype_char)) {
    if (is.na(genotype_char[i])) next

    # Remove any whitespace and convert to uppercase
    clean_genotype <- toupper(gsub("\\s+", "", genotype_char[i]))

    if (format == "standard") {
      # Handle E2E3 format - just standardize case
      if (grepl("^[E]?[234][E]?[234]$", clean_genotype, ignore.case = TRUE)) {
        # Extract just the numbers
        numbers <- as.numeric(strsplit(gsub("[^234]", "", clean_genotype), "")[[1]])
        if (length(numbers) == 2) {
          # Create standardized format
          result[i] <- paste0("E", min(numbers), "E", max(numbers))
        }
      }
    } else if (format == "numeric") {
      # Handle 23, 34, etc. format
      if (grepl("^[234]{2}$", clean_genotype)) {
        # Extract digits
        digits <- as.numeric(strsplit(clean_genotype, "")[[1]])
        # Create standardized format
        result[i] <- paste0("E", min(digits), "E", max(digits))
      }
    } else if (format == "slash") {
      # Handle e2/e3, E3/E4 format
      if (grepl("[E]?[234][/-][E]?[234]", clean_genotype, ignore.case = TRUE)) {
        # Extract numbers
        numbers <- as.numeric(strsplit(gsub("[^234]", "", clean_genotype), "")[[1]])
        if (length(numbers) == 2) {
          # Create standardized format
          result[i] <- paste0("E", min(numbers), "E", max(numbers))
        }
      }
    }
  }

  return(result)
}
