# APOE genotype constants
VALID_APOE_GENOTYPES <- c("E2E2", "E2E3", "E2E4", "E3E3", "E3E4", "E4E4")
E4_CARRIER_GENOTYPES <- c("E2E4", "E3E4", "E4E4")

#' Genotype formatting to E2E3 format
#' @noRd
.format_apoe_genotype <- function(genotypes) {
  # Convert to character and uppercase
  clean <- toupper(as.character(genotypes))

  # Handle common formats
  result <- rep(NA_character_, length(clean))

  for (i in seq_along(clean)) {
    if (is.na(clean[i])) next

    # Remove spaces and common separators
    g <- gsub("[\\s/-]", "", clean[i])

    # Extract numbers (2, 3, 4)
    numbers <- as.numeric(unlist(regmatches(g, gregexpr("[234]", g))))

    if (length(numbers) == 2) {
      # Sort to ensure consistent order (E2E3, not E3E2)
      numbers <- sort(numbers)
      result[i] <- paste0("E", numbers[1], "E", numbers[2])
    }
  }
  return(result)
}

#' APOE e4 status parsing
#' @noRd
.parse_e4_status_simple <- function(status_values) {
  status_char <- tolower(as.character(status_values))

  # Common TRUE values
  true_vals <- c("1", "yes", "y", "true", "t", "positive", "pos")
  # Common FALSE values
  false_vals <- c("0", "no", "n", "false", "f", "negative", "neg")

  result <- ifelse(status_char %in% true_vals, TRUE,
                   ifelse(status_char %in% false_vals, FALSE, NA))

  return(result)
}

#' Genotype prediction from SNPs
#' @noRd
.predict_genotype_from_snps <- function(rs7412, rs429358) {
  # Simple lookup without complex table
  result <- rep(NA_character_, length(rs7412))

  for (i in seq_along(rs7412)) {
    if (is.na(rs7412[i]) || is.na(rs429358[i])) next

    # Direct mapping
    snp_combo <- paste(toupper(rs7412[i]), toupper(rs429358[i]), sep = "_")

    result[i] <- switch(snp_combo,
                        "TT_TT" = "E2E2",
                        "TT_CT" = "E2E3",
                        "TT_CC" = "E3E3",
                        "CT_CC" = "E3E4",
                        "CC_CC" = "E4E4",
                        NA_character_  # Any other combination
    )
  }
  return(result)
}


