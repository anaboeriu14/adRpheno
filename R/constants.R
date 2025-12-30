# APOE Genetics Constants

#' SNP to APOE genotype mapping
#' Maps rs7412 and rs429358 SNP combinations to APOE genotypes
#' @keywords internal
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

#' APOE genotype carrier groups
#' @keywords internal
.E4_CARRIER_GENOTYPES <- c("E2E4", "E3E4", "E4E4")

#' @keywords internal
.E2_CARRIER_GENOTYPES <- c("E2E2", "E2E3")


#' APOE risk group levels (in order for factor levels and reporting)
#' @keywords internal
.APOE_RISK_GROUPS <- c("e3/e3", "e2+", "e4+")
