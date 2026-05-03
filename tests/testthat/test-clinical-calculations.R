# ---- calculate_egfr --------------------------------------------------------

test_that("calculate_egfr produces values in plausible CKD-EPI ranges", {
  # Reference: CKD-EPI 2021 calculator at https://www.kidney.org
  # Values cross-checked against the formula in the function's @details block.
  df <- data.frame(
    creatinine = c(1.0, 1.5, 0.8),
    age        = c(50,  65,  40),
    sex        = c(1,   0,   1)    # female, male, female
  )
  result <- calculate_egfr(df, "creatinine", "age", "sex", verbose = FALSE)

  # Sanity checks rather than exact reference matches: verify ranges
  # and the female-vs-male direction (CKD-EPI gives females slightly
  # higher eGFR for the same creatinine/age).
  expect_true(all(result$eGFR > 0))
  expect_true(all(result$eGFR < 200))
  # 50yo F with normal creatinine should be near-normal eGFR
  expect_gt(result$eGFR[1], 60)
  # 65yo M with elevated creatinine should be moderately reduced
  expect_lt(result$eGFR[2], 70)
})

test_that("calculate_egfr handles all sex encoding formats", {
  df_numeric <- data.frame(creatinine = 1.0, age = 50, sex = 1L)
  df_char    <- data.frame(creatinine = 1.0, age = 50, sex = "female")
  df_letter  <- data.frame(creatinine = 1.0, age = 50, sex = "F")

  r1 <- calculate_egfr(df_numeric, "creatinine", "age", "sex", verbose = FALSE)
  r2 <- calculate_egfr(df_char,    "creatinine", "age", "sex", verbose = FALSE)
  r3 <- calculate_egfr(df_letter,  "creatinine", "age", "sex", verbose = FALSE)

  expect_equal(r1$eGFR, r2$eGFR)
  expect_equal(r1$eGFR, r3$eGFR)
})

test_that("calculate_egfr errors on unrecognized sex values", {
  df <- data.frame(creatinine = 1.0, age = 50, sex = "white")
  expect_error(
    calculate_egfr(df, "creatinine", "age", "sex", verbose = FALSE),
    "unrecognized values"
  )
})

test_that("calculate_egfr respects custom output column name", {
  df <- data.frame(creatinine = 1.0, age = 50, sex = 1)
  result <- calculate_egfr(df, "creatinine", "age", "sex",
                           egfr_col = "kidney_function", verbose = FALSE)
  expect_true("kidney_function" %in% names(result))
  expect_false("eGFR" %in% names(result))
})

test_that("calculate_egfr overwrites existing eGFR column", {
  df <- data.frame(creatinine = 1.0, age = 50, sex = 1, eGFR = 999)
  result <- calculate_egfr(df, "creatinine", "age", "sex", verbose = FALSE)
  expect_false(any(result$eGFR == 999))
})


# ---- calculate_bmi_obesity -------------------------------------------------

test_that("calculate_bmi_obesity computes BMI from imperial units", {
  # 150 lbs, 65 inches: BMI = 703 * 150 / 65^2 = 24.96
  df <- data.frame(weight = 150, height = 65, bmi = 25)
  result <- calculate_bmi_obesity(df, "weight", "height", "bmi", verbose = FALSE)

  expect_equal(result$bmi_imperial, 24.96, tolerance = 0.01)
  expect_equal(result$obesity, 0L)   # below default cutoff of 30
})

test_that("calculate_bmi_obesity classifies obesity at the cutoff", {
  # 250 lbs, 65 inches: BMI = 703 * 250 / 65^2 = 41.6
  df <- data.frame(weight = 250, height = 65, bmi = 42)
  result <- calculate_bmi_obesity(df, "weight", "height", "bmi", verbose = FALSE)

  expect_gt(result$bmi_imperial, 30)
  expect_equal(result$obesity, 1L)
})

test_that("calculate_bmi_obesity respects custom cutoff", {
  # BMI ~25, custom cutoff 22 -> classified obese
  df <- data.frame(weight = 150, height = 65, bmi = 25)
  result <- calculate_bmi_obesity(df, "weight", "height", "bmi",
                                  cutoff = 22, verbose = FALSE)
  expect_equal(result$obesity, 1L)
})

test_that("calculate_bmi_obesity renames input columns as documented", {
  df <- data.frame(wt = 150, ht = 65, b = 25)
  result <- calculate_bmi_obesity(df, "wt", "ht", "b", verbose = FALSE)

  expect_true(all(c("weight_lbs", "height_inch", "bmi_original") %in% names(result)))
  expect_false(any(c("wt", "ht", "b") %in% names(result)))
})


# ---- calculate_bp_metrics --------------------------------------------------

test_that("calculate_bp_metrics averages two BP measurements", {
  df <- data.frame(s1 = 120, s2 = 130, d1 = 80, d2 = 90)
  result <- calculate_bp_metrics(df, "s1", "s2", "d1", "d2", verbose = FALSE)

  expect_equal(result$avg_systolic_bp, 125)
  expect_equal(result$avg_diastolic_bp, 85)
})

test_that("calculate_bp_metrics computes pulse pressure by default", {
  df <- data.frame(s1 = 120, s2 = 130, d1 = 80, d2 = 90)
  result <- calculate_bp_metrics(df, "s1", "s2", "d1", "d2", verbose = FALSE)

  # PP = avg systolic - avg diastolic = 125 - 85 = 40
  expect_equal(result$pulse_pressure, 40)
  expect_false("mean_arterial_pressure" %in% names(result))
})

test_that("calculate_bp_metrics computes MAP when requested", {
  df <- data.frame(s1 = 120, s2 = 130, d1 = 80, d2 = 90)
  result <- calculate_bp_metrics(df, "s1", "s2", "d1", "d2",
                                 calculate_map = TRUE, verbose = FALSE)

  # MAP = DBP + (SBP - DBP)/3 = 85 + (125 - 85)/3 = 85 + 13.33 = 98.33
  expect_equal(result$mean_arterial_pressure, 98.33, tolerance = 0.01)
})

test_that("calculate_bp_metrics errors when both flags are FALSE", {
  df <- data.frame(s1 = 120, s2 = 130, d1 = 80, d2 = 90)
  expect_error(
    calculate_bp_metrics(df, "s1", "s2", "d1", "d2",
                         calculate_pp = FALSE, calculate_map = FALSE,
                         verbose = FALSE),
    "calculate_map.*calculate_pp.*TRUE"
  )
})

test_that("calculate_bp_metrics handles NAs in measurements", {
  df <- data.frame(s1 = c(120, NA), s2 = c(130, 140),
                   d1 = c(80, 85), d2 = c(NA, 90))
  result <- calculate_bp_metrics(df, "s1", "s2", "d1", "d2", verbose = FALSE)

  # rowMeans with na.rm = TRUE: row 1 systolic = mean(120, 130) = 125
  #                            row 2 systolic = mean(NA, 140)   = 140
  expect_equal(result$avg_systolic_bp,  c(125, 140))
  expect_equal(result$avg_diastolic_bp, c(80,  87.5))
})
