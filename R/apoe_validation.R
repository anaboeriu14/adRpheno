# ---- APOE constants --------------------------------------------------------
# Canonical genotype form: "E{a}E{b}" with a <= b, both in {2, 3, 4},
# uppercase E. All comparisons assume input has been normalized through
# .format_apoe_genotype() first.

#' SNP to APOE genotype mapping
#'
#' Maps rs7412 + rs429358 SNP combinations to canonical APOE genotypes.
#' @keywords internal
#' @noRd
.SNP_GENOTYPE_MAP <- list(
  "TT_TT" = "E2E2",
  "CT_TT" = "E2E3",
  "TT_CT" = "E2E3",
  "CC_TT" = "E3E3",
  "TT_CC" = "E3E3",
  "CT_CT" = "E2E4",
  "CC_CT" = "E3E4",
  "CT_CC" = "E3E4",
  "CC_CC" = "E4E4"
)

#' APOE genotypes that confer e4 carrier status
#' @keywords internal
#' @noRd
.E4_CARRIER_GENOTYPES <- c("E2E4", "E3E4", "E4E4")

#' APOE genotypes that confer e2 carrier status
#' @keywords internal
#' @noRd
.E2_CARRIER_GENOTYPES <- c("E2E2", "E2E3")

#' APOE risk group factor levels (in display/reporting order)
#' @keywords internal
#' @noRd
.APOE_RISK_GROUPS <- c("e3/e3", "e2+", "e4+")

#' Validate APOE e4 carrier status
#'
#' Checks for mismatches between APOE genotypes and e4 carrier status flags.
#'
#' @param dataf Data frame containing APOE genotypes and e4 carrier status
#' @param genotype Character. Name of the APOE genotype column
#' @param e4_positive Character. Name of the e4 carrier status column
#' @param return_all Logical. Return entire dataset with validation column
#'   (default: `FALSE`)
#'
#' @return When `return_all = FALSE`: a data frame of mismatch rows, or
#'   invisible `NULL` if none. When `return_all = TRUE`: the full data frame
#'   with the added validation column.
#' @export
#'
#' @examples
#' \dontrun{
#' # Mismatches only
#' mismatches <- validate_apoe_e4_status(df, "apoe_genotype", "is_e4_carrier")
#'
#' # Full dataset with validation column
#' all_data <- validate_apoe_e4_status(df, "apoe_genotype", "is_e4_carrier",
#'                                     return_all = TRUE)
#' }
validate_apoe_e4_status <- function(dataf, genotype, e4_positive, return_all = FALSE) {

  adRutils::validate_args(
    data        = dataf,
    columns     = c(genotype, e4_positive),
    genotype    = adRutils::is_string(),
    e4_positive = adRutils::is_string(),
    return_all  = adRutils::is_flag()
  )

  result_df <- .add_e4_validation_columns(dataf, genotype, e4_positive)

  .return_validation_results(
    result_df       = result_df,
    original_df     = dataf,
    validation_col  = "e4_status_valid",
    return_all      = return_all,
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
#' @param return_all Logical. Return entire dataset with validation column
#'   (default: `FALSE`)
#'
#' @return When `return_all = FALSE`: a data frame of mismatch rows, or
#'   invisible `NULL` if none. When `return_all = TRUE`: the full data frame
#'   with the added validation column.
#' @export
#'
#' @examples
#' \dontrun{
#' mismatches <- match_snp_genotype(df, "rs7412", "rs429358", "apoe_genotype")
#' }
match_snp_genotype <- function(dataf, rs7412_col, rs429358_col, genotype_col,
                               return_all = FALSE) {

  adRutils::validate_args(
    data         = dataf,
    columns      = c(rs7412_col, rs429358_col, genotype_col),
    rs7412_col   = adRutils::is_string(),
    rs429358_col = adRutils::is_string(),
    genotype_col = adRutils::is_string(),
    return_all   = adRutils::is_flag()
  )

  result_df <- .add_snp_validation_columns(dataf, rs7412_col, rs429358_col, genotype_col)

  .return_validation_results(
    result_df       = result_df,
    original_df     = dataf,
    validation_col  = "genotype_match",
    return_all      = return_all,
    validation_type = "SNP-genotype"
  )
}


#' Classify APOE genotypes into risk groups
#'
#' Creates risk categories: e2+ (protective), e3/e3 (reference), e4+ (increased risk).
#' If `group_col` already exists in `dataf`, it is overwritten with a warning.
#'
#' @param dataf Data frame containing APOE genotype data
#' @param genotype_col Character. Name of the genotype column
#' @param group_col Character. Name for new risk group column
#'   (default: `"apoe_risk_group"`)
#' @param verbose Logical. Show informative messages (default: `TRUE`)
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

  adRutils::validate_args(
    data         = dataf,
    columns      = genotype_col,
    genotype_col = adRutils::is_string(),
    group_col    = adRutils::is_string(),
    verbose      = adRutils::is_flag()
  )

  if (group_col %in% names(dataf) && verbose) {
    cli::cli_alert_info("Column '{group_col}' already exists and will be overwritten")
  }

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

  result_df
}


# ---- internal helpers ------------------------------------------------------

#' Format APOE genotypes to standard form (E2E3, E3E4, ...)
#'
#' Vectorized: extracts the `2`/`3`/`4` digits from each input, sorts them,
#' and concatenates as `E{a}E{b}`. Returns `NA` when the input does not
#' resolve to exactly two digits.
#' @keywords internal
#' @noRd
.format_apoe_genotype <- function(genotypes) {
  clean <- gsub("[ /\\-]", "", toupper(as.character(genotypes)))
  digit_lists <- regmatches(clean, gregexpr("[234]", clean))

  vapply(digit_lists, function(d) {
    if (length(d) != 2L) return(NA_character_)
    d <- sort(as.integer(d))
    paste0("E", d[1], "E", d[2])
  }, character(1))
}


#' Parse e4 carrier status from various formats
#' @keywords internal
#' @noRd
.parse_e4_status <- function(status_values) {
  status_char <- tolower(as.character(status_values))

  true_vals  <- c("1", "yes", "y", "true",  "t", "positive", "pos", "carrier")
  false_vals <- c("0", "no",  "n", "false", "f", "negative", "neg", "non-carrier")

  ifelse(status_char %in% true_vals,  TRUE,
         ifelse(status_char %in% false_vals, FALSE, NA))
}


#' Predict APOE genotype from SNP combinations
#'
#' Vectorized lookup against `.SNP_GENOTYPE_MAP`.
#' @keywords internal
#' @noRd
.predict_genotype_from_snps <- function(rs7412, rs429358) {
  combos <- paste(toupper(rs7412), toupper(rs429358), sep = "_")
  vapply(combos, function(k) {
    val <- .SNP_GENOTYPE_MAP[[k]]
    if (is.null(val)) NA_character_ else val
  }, character(1), USE.NAMES = FALSE)
}


#' Classify APOE genotype into risk group
#' @keywords internal
#' @noRd
.classify_risk_group <- function(genotype) {
  ifelse(genotype %in% .E2_CARRIER_GENOTYPES, "e2+",
         ifelse(genotype == "E3E3",                  "e3/e3",
                ifelse(genotype %in% .E4_CARRIER_GENOTYPES, "e4+", NA_character_)))
}


#' Add e4 validation columns
#' @keywords internal
#' @noRd
.add_e4_validation_columns <- function(dataf, genotype_col, e4_col) {
  dataf %>%
    dplyr::mutate(
      formatted_genotype = .format_apoe_genotype(.data[[genotype_col]]),
      e4_status          = .parse_e4_status(.data[[e4_col]]),
      expected_e4        = .data$formatted_genotype %in% .E4_CARRIER_GENOTYPES,
      e4_status_valid    = .data$e4_status == .data$expected_e4
    )
}


#' Add SNP validation columns
#' @keywords internal
#' @noRd
.add_snp_validation_columns <- function(dataf, rs7412_col, rs429358_col, genotype_col) {
  dataf %>%
    dplyr::mutate(
      rs7412_clean       = toupper(as.character(.data[[rs7412_col]])),
      rs429358_clean     = toupper(as.character(.data[[rs429358_col]])),
      predicted_genotype = .predict_genotype_from_snps(.data$rs7412_clean,
                                                       .data$rs429358_clean),
      input_genotype_std = .format_apoe_genotype(.data[[genotype_col]]),
      genotype_match     = dplyr::if_else(
        !is.na(.data$predicted_genotype) & !is.na(.data$input_genotype_std),
        .data$predicted_genotype == .data$input_genotype_std,
        NA
      )
    )
}


#' Return validation results with consistent messaging
#' @keywords internal
#' @noRd
.return_validation_results <- function(result_df, original_df, validation_col,
                                       return_all, validation_type) {
  keep_cols <- c(names(original_df),
                 setdiff(names(result_df), names(original_df)))

  if (return_all) {
    return(result_df %>% dplyr::select(dplyr::all_of(keep_cols)))
  }

  mismatches <- which(result_df[[validation_col]] == FALSE)

  if (length(mismatches) == 0L) {
    cli::cli_alert_success("No {validation_type} mismatches found")
    return(invisible(NULL))
  }

  cli::cli_alert_warning("Found {length(mismatches)} {validation_type} mismatch{?es}")
  result_df[mismatches, keep_cols]
}


#' Report risk group classification
#' @keywords internal
#' @noRd
.report_risk_classification <- function(result_df, group_col) {
  classified_count <- sum(!is.na(result_df[[group_col]]))

  if (classified_count == 0L) {
    cli::cli_alert_warning("No genotypes could be classified")
    return(invisible(NULL))
  }

  group_counts <- table(result_df[[group_col]], useNA = "no")
  cli::cli_alert_success("Classified {classified_count} APOE risk group{?s}:")

  for (grp in .APOE_RISK_GROUPS) {
    if (grp %in% names(group_counts)) {
      pct <- round(100 * group_counts[grp] / classified_count, 1)
      cli::cli_alert_info("  {grp}: {group_counts[grp]} ({pct}%)")
    }
  }
}
