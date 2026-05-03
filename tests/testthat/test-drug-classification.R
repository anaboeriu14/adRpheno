# Note on word-boundary matching:
#   categorize_drugs() uses \b(pattern)\b, which means "statin" matches the
#   standalone word "statin" but NOT "atorvastatin" (the substring "statin"
#   is preceded by a word character "a", so no boundary). This is intentional
#   to avoid false positives. Tests below use input strings that exercise the
#   match behavior the function is actually designed for.

# ---- categorize_drugs (defaults) -------------------------------------------

test_that("categorize_drugs adds default cardiometabolic indicator columns", {
  df <- data.frame(
    medication = c("statin therapy", "metformin", "lisinopril", "tylenol")
  )
  result <- categorize_drugs(df, "medication")

  expect_true(all(c("is_cholesterol_med",
                    "is_diabetes_med",
                    "is_hypertension_med") %in% names(result)))
})

test_that("categorize_drugs matches the standalone words 'statin' and 'statins'", {
  # Word-boundary matching: 'statin' matches but 'atorvastatin' does not.
  df <- data.frame(medication = c("a statin", "statins are common", "atorvastatin"))
  result <- categorize_drugs(df, "medication")

  # Rows 1 and 2 contain "statin" / "statins" as standalone words. Row 3 has
  # "statin" only as a suffix of "atorvastatin" -> no match.
  expect_equal(result$is_cholesterol_med, c(1L, 1L, 0L))
})

test_that("categorize_drugs matches against ATC2 column when provided", {
  df <- data.frame(
    medication = c("brand_name_a", "brand_name_b"),
    atc2       = c("lipid modifying agents", "drugs used in diabetes")
  )
  result <- categorize_drugs(df, "medication", atc_col = "atc2")

  expect_equal(result$is_cholesterol_med, c(1L, 0L))
  expect_equal(result$is_diabetes_med,    c(0L, 1L))
})

test_that("categorize_drugs flags row when match in either column", {
  df <- data.frame(
    medication = c("statin therapy", "unknown_brand"),
    atc2       = c("unmapped",       "lipid modifying agents")
  )
  result <- categorize_drugs(df, "medication", atc_col = "atc2")

  # Row 1 matches via medication, row 2 matches via atc2
  expect_equal(result$is_cholesterol_med, c(1L, 1L))
})

test_that("categorize_drugs handles NA values in either column", {
  df <- data.frame(
    medication = c("statin",                NA),
    atc2       = c(NA,                      "lipid modifying agents")
  )
  result <- categorize_drugs(df, "medication", atc_col = "atc2")
  expect_equal(result$is_cholesterol_med, c(1L, 1L))
})


# ---- categorize_drugs (custom categories) ----------------------------------

test_that("categorize_drugs applies user-supplied categories", {
  df <- data.frame(medication = c("warfarin",
                                  "apixaban",
                                  "sertraline",
                                  "tylenol"))

  custom <- list(
    anticoagulant  = c("warfarin", "apixaban", "rivaroxaban"),
    antidepressant = c("sertraline", "fluoxetine")
  )
  result <- categorize_drugs(df, "medication", categories = custom)

  expect_true(all(c("is_anticoagulant_med",
                    "is_antidepressant_med") %in% names(result)))
  # Defaults should NOT be present when custom categories given
  expect_false("is_cholesterol_med" %in% names(result))
  expect_false("is_diabetes_med"    %in% names(result))

  expect_equal(result$is_anticoagulant_med,  c(1L, 1L, 0L, 0L))
  expect_equal(result$is_antidepressant_med, c(0L, 0L, 1L, 0L))
})

test_that("categorize_drugs respects custom prefix and suffix", {
  df <- data.frame(medication = "statin")
  result <- categorize_drugs(df, "medication",
                             prefix = "has_", suffix = "")

  expect_true("has_cholesterol" %in% names(result))
  expect_false("is_cholesterol_med" %in% names(result))
})

test_that("categorize_drugs accepts empty prefix and suffix", {
  df <- data.frame(medication = "statin")
  result <- categorize_drugs(df, "medication",
                             prefix = "", suffix = "")

  expect_true("cholesterol" %in% names(result))
})

test_that("categorize_drugs uses word-boundary matching to avoid false positives", {
  # "vastatinoid" should NOT match because "statin" is not a whole word in it
  # (the 's' is preceded by 'a', a word character, so no \b before "statin").
  df <- data.frame(medication = c("statins-are-good", "vastatinoid"))
  result <- categorize_drugs(df, "medication")

  expect_equal(result$is_cholesterol_med[1], 1L)   # matches "statins"
  expect_equal(result$is_cholesterol_med[2], 0L)   # "statin" not whole-word
})

test_that("categorize_drugs is case-insensitive", {
  df <- data.frame(medication = c("STATIN", "Diuretics", "tylenol"))
  result <- categorize_drugs(df, "medication")

  expect_equal(result$is_cholesterol_med[1],  1L)   # "STATIN" matches
  expect_equal(result$is_hypertension_med[2], 1L)   # "Diuretics" matches
  expect_equal(result$is_diabetes_med[3],     0L)   # tylenol matches nothing
})
