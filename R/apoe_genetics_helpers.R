# APOE genotype constants
VALID_APOE_GENOTYPES <- c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4")
E4_CARRIER_GENOTYPES <- c("E2E4", "E3E4", "E4E4")

#' Clean SNP genotype values to handle CT/TC equivalency
#' @param snp_vector Vector of SNP values
#' @return Cleaned SNP values with TC converted to CT
#' @keywords internal
.clean_snp_genotype_values <- function(snp_vector) {
  snp_clean <- toupper(as.character(snp_vector))
  # Convert TC to CT for consistency
  snp_clean[snp_clean == "TC"] <- "CT"
  return(snp_clean)
}

#' Create SNP to APOE genotype lookup table
#' @return Data frame with SNP combinations and predicted APOE genotypes
#' @keywords internal
.create_snp_to_apoe_lookup <- function() {
  data.frame(
    rs7412 = c("TT", "TT", "CT", "TT", "CT", "CC"),
    rs429358 = c("TT", "CT", "CT", "CC", "CC", "CC"),
    predicted_genotype = c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4"),
    stringsAsFactors = FALSE
  )
}

#' Predict APOE genotype from SNP values
#' @param rs7412_values Vector of rs7412 values
#' @param rs429358_values Vector of rs429358 values
#' @param lookup_table Lookup table for SNP combinations
#' @return Vector of predicted APOE genotypes
#' @keywords internal
.predict_apoe_from_snps <- function(rs7412_values, rs429358_values, lookup_table) {
  predicted <- rep(NA_character_, length(rs7412_values))

  for (i in seq_along(rs7412_values)) {
    if (is.na(rs7412_values[i]) || is.na(rs429358_values[i])) next

    match_idx <- which(lookup_table$rs7412 == rs7412_values[i] &
                         lookup_table$rs429358 == rs429358_values[i])

    if (length(match_idx) == 1) {
      predicted[i] <- lookup_table$predicted_genotype[match_idx]
    }
  }

  return(predicted)
}

#' Classify APOE genotype to risk group
#' @param std_genotypes Vector of standardized APOE genotypes
#' @return Vector of risk group classifications
#' @keywords internal
.classify_apoe_to_risk_group <- function(std_genotypes) {
  risk_groups <- rep(NA_character_, length(std_genotypes))

  # e2+ (potentially protective)
  risk_groups[std_genotypes %in% c("E2E2", "E2E3")] <- "e2+"

  # e3/e3 (reference)
  risk_groups[std_genotypes == "E3E3"] <- "e3/e3"

  # e4+ (increased risk)
  risk_groups[std_genotypes %in% c("E2E4", "E3E4", "E4E4")] <- "e4+"

  return(risk_groups)
}

#' Parse e4 carrier status values to logical
#' @param status_vector Vector of e4 status values
#' @param true_values Vector of values that represent e4 positive
#' @param false_values Vector of values that represent e4 negative
#' @return Logical vector of standardized e4 status
#' @keywords internal
.parse_e4_carrier_status <- function(status_vector, true_values, false_values) {
  status_char <- as.character(status_vector)
  result <- rep(NA, length(status_char))

  true_values_lower <- tolower(as.character(true_values))
  false_values_lower <- tolower(as.character(false_values))

  for (i in seq_along(status_char)) {
    if (is.na(status_char[i])) next

    value_lower <- tolower(status_char[i])

    if (value_lower %in% true_values_lower) {
      result[i] <- TRUE
    } else if (value_lower %in% false_values_lower) {
      result[i] <- FALSE
    }
  }

  return(result)
}

#' Format APOE genotype to standard E2E3 notation
#' @param genotype_vector Vector of genotype values to format
#' @param format Format of the input genotypes
#' @return Vector of formatted genotypes in E2E3 format
#' @keywords internal
.format_apoe_genotype <- function(genotype_vector, format = c("standard", "numeric", "slash")) {
  format <- match.arg(format)
  genotype_char <- as.character(genotype_vector)
  result <- rep(NA_character_, length(genotype_char))

  for (i in seq_along(genotype_char)) {
    if (is.na(genotype_char[i])) next

    clean_genotype <- toupper(gsub("\\s+", "", genotype_char[i]))

    if (format == "standard") {
      if (grepl("^[E]?[234][E]?[234]$", clean_genotype, ignore.case = TRUE)) {
        numbers <- as.numeric(strsplit(gsub("[^234]", "", clean_genotype), "")[[1]])
        if (length(numbers) == 2) {
          result[i] <- paste0("E", min(numbers), "E", max(numbers))
        }
      }
    } else if (format == "numeric") {
      if (grepl("^[234]{2}$", clean_genotype)) {
        digits <- as.numeric(strsplit(clean_genotype, "")[[1]])
        result[i] <- paste0("E", min(digits), "E", max(digits))
      }
    } else if (format == "slash") {
      if (grepl("[E]?[234][/-][E]?[234]", clean_genotype, ignore.case = TRUE)) {
        numbers <- as.numeric(strsplit(gsub("[^234]", "", clean_genotype), "")[[1]])
        if (length(numbers) == 2) {
          result[i] <- paste0("E", min(numbers), "E", max(numbers))
        }
      }
    }
  }
  return(result)
}
