# ---- validate_apoe_e4_status -----------------------------------------------

test_that("validate_apoe_e4_status returns NULL when all e4 flags are correct", {
  df <- data.frame(
    id          = 1:4,
    apoe        = c("E2E3", "E3E3", "E3E4", "E4E4"),
    e4_carrier  = c(0,      0,      1,      1)
  )
  result <- validate_apoe_e4_status(df, "apoe", "e4_carrier")
  expect_null(result)
})

test_that("validate_apoe_e4_status flags rows with mismatched e4 status", {
  df <- data.frame(
    id          = 1:4,
    apoe        = c("E2E3", "E3E3", "E3E4", "E4E4"),
    e4_carrier  = c(1,      0,      0,      1)    # rows 1 and 3 mismatched
  )
  result <- validate_apoe_e4_status(df, "apoe", "e4_carrier")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(1, 3))
})

test_that("validate_apoe_e4_status return_all keeps all rows with validation column", {
  df <- data.frame(
    id          = 1:3,
    apoe        = c("E3E3", "E3E4", "E4E4"),
    e4_carrier  = c(0,      0,      1)         # row 2 is mismatched
  )
  result <- validate_apoe_e4_status(df, "apoe", "e4_carrier", return_all = TRUE)

  expect_equal(nrow(result), 3)
  expect_true("e4_status_valid" %in% names(result))
  expect_equal(result$e4_status_valid, c(TRUE, FALSE, TRUE))
})


# ---- match_snp_genotype ----------------------------------------------------

test_that("match_snp_genotype finds matching SNP-genotype pairs", {
  df <- data.frame(
    id       = 1:3,
    rs7412   = c("TT", "CC", "CT"),
    rs429358 = c("TT", "CC", "CT"),
    apoe     = c("E2E2", "E4E4", "E2E4")
  )
  result <- match_snp_genotype(df, "rs7412", "rs429358", "apoe")
  expect_null(result)
})

test_that("match_snp_genotype flags inconsistent SNP-genotype pairs", {
  df <- data.frame(
    id       = 1:3,
    rs7412   = c("TT", "CC", "CT"),
    rs429358 = c("TT", "CC", "CT"),
    apoe     = c("E2E2", "E2E2", "E2E4")     # row 2 mismatched
  )
  result <- match_snp_genotype(df, "rs7412", "rs429358", "apoe")

  expect_equal(nrow(result), 1)
  expect_equal(result$id, 2)
})


# ---- classify_apoe_risk_groups ---------------------------------------------

test_that("classify_apoe_risk_groups assigns the three risk groups", {
  df <- data.frame(
    apoe = c("E2E2", "E2E3", "E3E3", "E3E4", "E4E4")
  )
  result <- classify_apoe_risk_groups(df, "apoe", verbose = FALSE)

  expect_equal(as.character(result$apoe_risk_group),
               c("e2+", "e2+", "e3/e3", "e4+", "e4+"))
})

test_that("classify_apoe_risk_groups produces an ordered factor", {
  df <- data.frame(apoe = c("E3E3", "E3E4", "E2E3"))
  result <- classify_apoe_risk_groups(df, "apoe", verbose = FALSE)

  expect_s3_class(result$apoe_risk_group, "factor")
  expect_equal(levels(result$apoe_risk_group), c("e3/e3", "e2+", "e4+"))
})

test_that("classify_apoe_risk_groups handles malformed genotypes as NA", {
  df <- data.frame(apoe = c("E3E3", "garbage", NA, "E4E4"))
  result <- classify_apoe_risk_groups(df, "apoe", verbose = FALSE)

  expect_equal(as.character(result$apoe_risk_group),
               c("e3/e3", NA, NA, "e4+"))
})

test_that("classify_apoe_risk_groups overwrites existing column with warning", {
  df <- data.frame(apoe = "E3E3", apoe_risk_group = "stale_value")
  expect_message(
    result <- classify_apoe_risk_groups(df, "apoe", verbose = TRUE),
    "already exists"
  )
  expect_equal(as.character(result$apoe_risk_group), "e3/e3")
})

test_that("classify_apoe_risk_groups respects custom output column name", {
  df <- data.frame(apoe = "E3E4")
  result <- classify_apoe_risk_groups(df, "apoe",
                                      group_col = "risk_cat", verbose = FALSE)
  expect_true("risk_cat" %in% names(result))
  expect_false("apoe_risk_group" %in% names(result))
})
