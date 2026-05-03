# ---- .format_apoe_genotype -------------------------------------------------

test_that(".format_apoe_genotype handles standard genotype strings", {
  expect_equal(adRpheno:::.format_apoe_genotype("E2E3"), "E2E3")
  expect_equal(adRpheno:::.format_apoe_genotype("E3E4"), "E3E4")
  expect_equal(adRpheno:::.format_apoe_genotype("E4E4"), "E4E4")
})

test_that(".format_apoe_genotype canonicalizes order (smaller digit first)", {
  expect_equal(adRpheno:::.format_apoe_genotype("E4E2"), "E2E4")
  expect_equal(adRpheno:::.format_apoe_genotype("E4E3"), "E3E4")
  expect_equal(adRpheno:::.format_apoe_genotype("E3E2"), "E2E3")
})

test_that(".format_apoe_genotype handles lowercase and separators", {
  expect_equal(adRpheno:::.format_apoe_genotype("e2e3"), "E2E3")
  expect_equal(adRpheno:::.format_apoe_genotype("e2/e3"), "E2E3")
  expect_equal(adRpheno:::.format_apoe_genotype("e2 e3"), "E2E3")
  expect_equal(adRpheno:::.format_apoe_genotype("e2-e3"), "E2E3")
  expect_equal(adRpheno:::.format_apoe_genotype("E4/E4"), "E4E4")
})

test_that(".format_apoe_genotype handles digit-only inputs", {
  expect_equal(adRpheno:::.format_apoe_genotype("23"), "E2E3")
  expect_equal(adRpheno:::.format_apoe_genotype("4/4"), "E4E4")
  expect_equal(adRpheno:::.format_apoe_genotype("3-4"), "E3E4")
})

test_that(".format_apoe_genotype is vectorized", {
  input  <- c("E2E3", "e3/e4", "E4E4", NA, "garbage")
  output <- adRpheno:::.format_apoe_genotype(input)
  expect_equal(output, c("E2E3", "E3E4", "E4E4", NA, NA))
})

test_that(".format_apoe_genotype returns NA for malformed inputs", {
  expect_true(is.na(adRpheno:::.format_apoe_genotype("E5E5")))   # no 5 allele
  expect_true(is.na(adRpheno:::.format_apoe_genotype("E2")))     # only one digit
  expect_true(is.na(adRpheno:::.format_apoe_genotype("")))
  expect_true(is.na(adRpheno:::.format_apoe_genotype(NA)))
  expect_true(is.na(adRpheno:::.format_apoe_genotype("E2E3E4"))) # three digits
})


# ---- .predict_genotype_from_snps -------------------------------------------

test_that(".predict_genotype_from_snps returns canonical genotypes for known SNP combos", {
  # Spot-check a few entries from .SNP_GENOTYPE_MAP
  expect_equal(adRpheno:::.predict_genotype_from_snps("TT", "TT"), "E2E2")
  expect_equal(adRpheno:::.predict_genotype_from_snps("CC", "CC"), "E4E4")
  expect_equal(adRpheno:::.predict_genotype_from_snps("CC", "TT"), "E3E3")
  expect_equal(adRpheno:::.predict_genotype_from_snps("CT", "CT"), "E2E4")
})

test_that(".predict_genotype_from_snps handles lowercase input", {
  expect_equal(adRpheno:::.predict_genotype_from_snps("tt", "tt"), "E2E2")
  expect_equal(adRpheno:::.predict_genotype_from_snps("cc", "cc"), "E4E4")
})

test_that(".predict_genotype_from_snps is vectorized", {
  rs7412   <- c("TT", "CC", "CT", "AA")    # AA is invalid
  rs429358 <- c("TT", "CC", "CT", "TT")
  result   <- adRpheno:::.predict_genotype_from_snps(rs7412, rs429358)
  expect_equal(result, c("E2E2", "E4E4", "E2E4", NA))
})

test_that(".predict_genotype_from_snps returns NA for unrecognized combos", {
  expect_true(is.na(adRpheno:::.predict_genotype_from_snps("AA", "GG")))
  expect_true(is.na(adRpheno:::.predict_genotype_from_snps("XX", "YY")))
})


# ---- .classify_risk_group --------------------------------------------------

test_that(".classify_risk_group assigns expected groups", {
  # e2+: E2E2, E2E3
  expect_equal(adRpheno:::.classify_risk_group("E2E2"), "e2+")
  expect_equal(adRpheno:::.classify_risk_group("E2E3"), "e2+")

  # e3/e3 (reference)
  expect_equal(adRpheno:::.classify_risk_group("E3E3"), "e3/e3")

  # e4+: E2E4, E3E4, E4E4
  expect_equal(adRpheno:::.classify_risk_group("E2E4"), "e4+")
  expect_equal(adRpheno:::.classify_risk_group("E3E4"), "e4+")
  expect_equal(adRpheno:::.classify_risk_group("E4E4"), "e4+")
})

test_that(".classify_risk_group is vectorized", {
  input  <- c("E3E3", "E3E4", "E2E2", "E2E4", NA, "garbage")
  output <- adRpheno:::.classify_risk_group(input)
  expect_equal(output, c("e3/e3", "e4+", "e2+", "e4+", NA, NA))
})


# ---- .parse_e4_status ------------------------------------------------------

test_that(".parse_e4_status recognizes positive encodings", {
  expect_true(adRpheno:::.parse_e4_status("1"))
  expect_true(adRpheno:::.parse_e4_status("yes"))
  expect_true(adRpheno:::.parse_e4_status("Y"))
  expect_true(adRpheno:::.parse_e4_status("TRUE"))
  expect_true(adRpheno:::.parse_e4_status("positive"))
  expect_true(adRpheno:::.parse_e4_status("carrier"))
})

test_that(".parse_e4_status recognizes negative encodings", {
  expect_false(adRpheno:::.parse_e4_status("0"))
  expect_false(adRpheno:::.parse_e4_status("no"))
  expect_false(adRpheno:::.parse_e4_status("N"))
  expect_false(adRpheno:::.parse_e4_status("FALSE"))
  expect_false(adRpheno:::.parse_e4_status("negative"))
  expect_false(adRpheno:::.parse_e4_status("non-carrier"))
})

test_that(".parse_e4_status returns NA for unrecognized values", {
  expect_true(is.na(adRpheno:::.parse_e4_status("maybe")))
  expect_true(is.na(adRpheno:::.parse_e4_status(NA)))
  expect_true(is.na(adRpheno:::.parse_e4_status("")))
})

test_that(".parse_e4_status is case-insensitive and vectorized", {
  input  <- c("YES", "no", "Carrier", "Non-Carrier", "junk")
  output <- adRpheno:::.parse_e4_status(input)
  expect_equal(output, c(TRUE, FALSE, TRUE, FALSE, NA))
})
