

#' Validate APOE e4 carrier status
#'
#' @param dataf A data frame containing APOE genotypes and e4 carrier status
#' @param genotype Character. The name of the APOE genotype column
#' @param e4_positive Character. The name of the e4 carrier status column
#' @param return_all Logical. If TRUE returns entire dataset with validation column (default: FALSE)
#'
#' @return Data frame with validation results or message if no mismatches
#' @export
validate_apoe_e4_status <- function(dataf, genotype, e4_positive, return_all = FALSE) {

  # === STEP 1: validation ===
  adRutils::validate_params(
    data = dataf,
    columns = c(genotype, e4_positive),
    custom_checks = list(
      list(
        condition = is.logical(return_all) && length(return_all) == 1,
        message = "return_all must be TRUE or FALSE"
      )
    ),
    context = "validate_apoe_e4_status"
  )

  # === STEP 2: Standardize inputs ===
  result_df <- dataf

  # Simple genotype formatting
  result_df$formatted_genotype <- .format_apoe_genotype(dataf[[genotype]])

  # Simple e4 status parsing (handles common TRUE/FALSE representations)
  result_df$e4_status <- .parse_e4_status_simple(dataf[[e4_positive]])

  # === STEP 3: Check for mismatches ===
  result_df$expected_e4 <- result_df$formatted_genotype %in% E4_CARRIER_GENOTYPES
  result_df$e4_status_valid <- result_df$e4_status == result_df$expected_e4

  # Find mismatches (excluding NAs)
  mismatches <- which(!result_df$e4_status_valid & !is.na(result_df$e4_status_valid))

  # === STEP 4: Return results ===
  if (length(mismatches) > 0) {
    if (return_all) {
      return(result_df[c(names(dataf), "formatted_genotype", "e4_status_valid")])
    } else {
      return(result_df[mismatches, c(names(dataf), "formatted_genotype", "e4_status_valid")])
    }
  } else {
    if (return_all) {
      return(result_df[c(names(dataf), "formatted_genotype", "e4_status_valid")])
    } else {
      return("No mismatches found")
    }
  }
}


#' Match APOE SNPs to genotypes
#'
#' @param dataf A data frame containing SNPs and genotypes
#' @param rs7412_col Character. Name of the rs7412 SNP column
#' @param rs429358_col Character. Name of the rs429358 SNP column
#' @param genotype_col Character. Name of the genotype column
#' @param return_all Logical. Return entire dataset or just mismatches (default: TRUE)
#'
#' @return Data frame with validation results
#' @export
match_snp_genotype <- function(dataf, rs7412_col, rs429358_col, genotype_col, return_all = TRUE) {

  # === STEP 1: Simple validation ===
  adRutils::validate_params(
    data = dataf,
    columns = c(rs7412_col, rs429358_col, genotype_col),
    custom_checks = list(
      list(
        condition = is.logical(return_all) && length(return_all) == 1,
        message = "return_all must be TRUE or FALSE"
      )
    ),
    context = "match_snp_genotype"
  )

  # === STEP 2: Process SNPs and predict genotype ===
  result_df <- dataf

  # Just uppercase the SNPs
  result_df$rs7412_clean <- toupper(as.character(dataf[[rs7412_col]]))
  result_df$rs429358_clean <- toupper(as.character(dataf[[rs429358_col]]))

  # Predict genotype from SNPs
  result_df$predicted_genotype <- .predict_genotype_from_snps(
    result_df$rs7412_clean,
    result_df$rs429358_clean
  )

  # Standardize input genotype
  result_df$input_genotype_std <- .format_apoe_genotype(dataf[[genotype_col]])

  # === STEP 3: Check matches ===
  result_df$genotype_match <- ifelse(
    !is.na(result_df$predicted_genotype) & !is.na(result_df$input_genotype_std),
    result_df$predicted_genotype == result_df$input_genotype_std,
    NA
  )

  # === STEP 4: Return results ===
  if (return_all) {
    return(result_df[c(names(dataf), "predicted_genotype", "genotype_match")])
  } else {
    mismatches <- which(result_df$genotype_match == FALSE)
    if (length(mismatches) > 0) {
      return(result_df[mismatches, c(names(dataf), "predicted_genotype", "genotype_match")])
    } else {
      return("No mismatches found")
    }
  }
}


#' Classify APOE genotypes into risk groups
#'
#' @param dataf A data frame containing APOE genotype data
#' @param genotype_col Character. Name of the genotype column
#' @param group_col Character. Name for new risk group column (default: "apoe_risk_group")
#'
#' @return Data frame with added risk group column
#' @export
classify_apoe_risk_groups <- function(dataf, genotype_col, group_col = "apoe_risk_group") {

  # === STEP 1: Simple validation ===
  adRutils::validate_params(
    data = dataf,
    columns = genotype_col,
    custom_checks = list(
      list(
        condition = is.character(group_col) && length(group_col) == 1,
        message = "group_col must be a single character string"
      ),
      list(
        condition = !group_col %in% names(dataf),
        message = paste0("Column '", group_col, "' already exists")
      )
    ),
    context = "classify_apoe_risk_groups"
  )

  # === STEP 2: Format and classify ===
  result_df <- dataf
  formatted_genotypes <- .format_apoe_genotype(dataf[[genotype_col]])

  # Simple risk group classification
  result_df[[group_col]] <- ifelse(
    formatted_genotypes %in% c("E2E2", "E2E3"), "e2+",
    ifelse(formatted_genotypes == "E3E3", "e3/e3",
           ifelse(formatted_genotypes %in% E4_CARRIER_GENOTYPES, "e4+", NA))
  )

  # Convert to factor with proper levels
  result_df[[group_col]] <- factor(result_df[[group_col]],
                                   levels = c("e3/e3", "e2+", "e4+"))

  return(result_df)
}


