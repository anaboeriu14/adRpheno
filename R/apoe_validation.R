#' Validate APOE e4 carrier status
#'
#' Checks for mismatches between APOE genotypes and e4 carrier status flags.
#'
#' @param dataf Data frame containing APOE genotypes and e4 carrier status
#' @param genotype Character. Name of the APOE genotype column
#' @param e4_positive Character. Name of the e4 carrier status column
#' @param return_all Logical. Return entire dataset with validation column (default: FALSE)
#'
#' @return Data frame with validation results, or invisible NULL if no mismatches
#' @export
#'
#' @examples
#' \dontrun{
#' # Check for mismatches only
#' mismatches <- validate_apoe_e4_status(df, "apoe_genotype", "is_e4_carrier")
#'
#' # Get all records with validation
#' all_data <- validate_apoe_e4_status(df, "apoe_genotype", "is_e4_carrier",
#'                                     return_all = TRUE)
#' }
validate_apoe_e4_status <- function(dataf, genotype, e4_positive, return_all = FALSE) {

  adRutils::validate_params(
    data = dataf,
    columns = c(genotype, e4_positive),
    custom_checks = list(
      list(
        condition = is.logical(return_all) && length(return_all) == 1,
        message = "{.arg return_all} must be TRUE or FALSE"
      )
    ),
    context = "validate_apoe_e4_status"
  )

  result_df <- .add_e4_validation_columns(dataf, genotype, e4_positive)

  .return_validation_results(
    result_df = result_df,
    original_df = dataf,
    validation_col = "e4_status_valid",
    return_all = return_all,
    validation_type = "e4 status"
  )
}


#' Match APOE SNPs to genotypes
#'
#' Validates that SNP combinations (rs7412, rs429358) match reported genotypes.
#'
#' @param dataf Data frame containing SNPs and genotypes
#' @param rs7412_col Character. Name of the rs7412 SNP column
#' @param rs429358_col Character. Name of the rs429358 SNP column
#' @param genotype_col Character. Name of the genotype column
#' @param return_all Logical. Return entire dataset or just mismatches (default: FALSE)
#'
#' @return Data frame with validation results, or invisible NULL if no mismatches
#' @export
#'
#' @examples
#' \dontrun{
#' mismatches <- match_snp_genotype(df, "rs7412", "rs429358", "apoe_genotype")
#' }
match_snp_genotype <- function(dataf, rs7412_col, rs429358_col, genotype_col,
                               return_all = FALSE) {

  adRutils::validate_params(
    data = dataf,
    columns = c(rs7412_col, rs429358_col, genotype_col),
    custom_checks = list(
      list(
        condition = is.logical(return_all) && length(return_all) == 1,
        message = "{.arg return_all} must be TRUE or FALSE"
      )
    ),
    context = "match_snp_genotype"
  )

  result_df <- .add_snp_validation_columns(dataf, rs7412_col, rs429358_col, genotype_col)

  .return_validation_results(
    result_df = result_df,
    original_df = dataf,
    validation_col = "genotype_match",
    return_all = return_all,
    validation_type = "SNP-genotype"
  )
}


#' Classify APOE genotypes into risk groups
#'
#' Creates risk categories: e2+ (protective), e3/e3 (reference), e4+ (increased risk).
#'
#' @param dataf Data frame containing APOE genotype data
#' @param genotype_col Character. Name of the genotype column
#' @param group_col Character. Name for new risk group column (default: "apoe_risk_group")
#' @param verbose Logical. Show informative messages (default: TRUE)
#'
#' @return Data frame with added risk group column (factor)
#' @export
#'
#' @examples
#' \dontrun{
#' df_risk <- classify_apoe_risk_groups(df, "apoe_genotype")
#' }
classify_apoe_risk_groups <- function(dataf, genotype_col,
                                      group_col = "apoe_risk_group",
                                      verbose = TRUE) {

  adRutils::validate_params(
    data = dataf,
    columns = genotype_col,
    custom_checks = list(
      list(
        condition = is.character(group_col) && length(group_col) == 1,
        message = "{.arg group_col} must be a single character string"
      ),
      list(
        condition = !group_col %in% names(dataf),
        message = "Column '{group_col}' already exists"
      ),
      list(
        condition = is.logical(verbose) && length(verbose) == 1,
        message = "{.arg verbose} must be TRUE or FALSE"
      )
    ),
    context = "classify_apoe_risk_groups"
  )

  result_df <- dataf %>%
    dplyr::mutate(
      formatted_genotype = .format_apoe_genotype(.data[[genotype_col]]),
      !!group_col := .classify_risk_group(.data$formatted_genotype),
      !!group_col := factor(.data[[group_col]], levels = .APOE_RISK_GROUPS)
    ) %>%
    dplyr::select(-"formatted_genotype")

  if (verbose) {
    .report_risk_classification(result_df, group_col)
  }

  return(result_df)
}

#' Format APOE genotype to standard format (E2E3, E3E4, etc.)
#' @keywords internal
.format_apoe_genotype <- function(genotypes) {
  clean <- toupper(as.character(genotypes))
  result <- rep(NA_character_, length(clean))

  for (i in seq_along(clean)) {
    if (is.na(clean[i])) next

    g <- gsub("[\\s/-]", "", clean[i])
    numbers <- as.numeric(unlist(regmatches(g, gregexpr("[234]", g))))

    if (length(numbers) == 2) {
      numbers <- sort(numbers)
      result[i] <- paste0("E", numbers[1], "E", numbers[2])
    }
  }

  return(result)
}

#' Parse e4 carrier status from various formats
#' @keywords internal
.parse_e4_status <- function(status_values) {
  status_char <- tolower(as.character(status_values))

  true_vals <- c("1", "yes", "y", "true", "t", "positive", "pos", "carrier")
  false_vals <- c("0", "no", "n", "false", "f", "negative", "neg", "non-carrier")

  ifelse(status_char %in% true_vals, TRUE,
         ifelse(status_char %in% false_vals, FALSE, NA))
}

#' Predict APOE genotype from SNP combinations
#' @keywords internal
.predict_genotype_from_snps <- function(rs7412, rs429358) {
  result <- rep(NA_character_, length(rs7412))

  for (i in seq_along(rs7412)) {
    if (is.na(rs7412[i]) || is.na(rs429358[i])) next

    snp_combo <- paste(toupper(rs7412[i]), toupper(rs429358[i]), sep = "_")
    result[i] <- .SNP_GENOTYPE_MAP[[snp_combo]]
  }

  return(result)
}

#' Classify APOE genotype into risk group
#' @keywords internal
.classify_risk_group <- function(genotype) {
  ifelse(genotype %in% .E2_CARRIER_GENOTYPES, "e2+",
         ifelse(genotype == "E3E3", "e3/e3",
                ifelse(genotype %in% .E4_CARRIER_GENOTYPES, "e4+", NA_character_)))
}

#' Add e4 validation columns
#' @keywords internal
.add_e4_validation_columns <- function(dataf, genotype_col, e4_col) {
  dataf %>%
    dplyr::mutate(
      formatted_genotype = .format_apoe_genotype(.data[[genotype_col]]),
      e4_status = .parse_e4_status(.data[[e4_col]]),
      expected_e4 = .data$formatted_genotype %in% .E4_CARRIER_GENOTYPES,
      e4_status_valid = .data$e4_status == .data$expected_e4
    )
}

#' Add SNP validation columns
#' @keywords internal
.add_snp_validation_columns <- function(dataf, rs7412_col, rs429358_col, genotype_col) {
  dataf %>%
    dplyr::mutate(
      rs7412_clean = toupper(as.character(.data[[rs7412_col]])),
      rs429358_clean = toupper(as.character(.data[[rs429358_col]])),
      predicted_genotype = .predict_genotype_from_snps(.data$rs7412_clean,
                                                       .data$rs429358_clean),
      input_genotype_std = .format_apoe_genotype(.data[[genotype_col]]),
      genotype_match = dplyr::if_else(
        !is.na(.data$predicted_genotype) & !is.na(.data$input_genotype_std),
        .data$predicted_genotype == .data$input_genotype_std,
        NA
      )
    )
}

#' Return validation results with consistent messaging
#' @keywords internal
.return_validation_results <- function(result_df, original_df, validation_col,
                                       return_all, validation_type) {
  mismatches <- which(result_df[[validation_col]] == FALSE)

  if (return_all) {
    keep_cols <- c(names(original_df),
                   setdiff(names(result_df), names(original_df)))
    return(result_df %>% dplyr::select(dplyr::all_of(keep_cols)))
  }

  if (length(mismatches) == 0) {
    cli::cli_alert_success("No {validation_type} mismatches found")
    return(invisible(NULL))
  }

  cli::cli_alert_warning("Found {length(mismatches)} {validation_type} mismatch{?es}")
  keep_cols <- c(names(original_df),
                 setdiff(names(result_df), names(original_df)))
  return(result_df[mismatches, keep_cols])
}

#' Report risk group classification
#' @keywords internal
.report_risk_classification <- function(result_df, group_col) {
  classified_count <- sum(!is.na(result_df[[group_col]]))

  if (classified_count == 0) {
    cli::cli_alert_warning("No genotypes could be classified")
    return(invisible(NULL))
  }

  group_counts <- table(result_df[[group_col]], useNA = "no")
  cli::cli_alert_success("Classified {classified_count} APOE risk groups:")

  # Use constant for consistent ordering
  for (grp in .APOE_RISK_GROUPS) {
    if (grp %in% names(group_counts)) {
      pct <- round(100 * group_counts[grp] / classified_count, 1)
      cli::cli_alert_info("  {grp}: {group_counts[grp]} ({pct}%)")
    }
  }
}
