# ---- .parse_sex ------------------------------------------------------------

test_that(".parse_sex handles numeric encodings", {
  expect_equal(adRpheno:::.parse_sex(c(0, 1)), c(0, 1))
  expect_equal(adRpheno:::.parse_sex(c(1, 0, 1, 0)), c(1, 0, 1, 0))
})

test_that(".parse_sex handles single-letter encodings", {
  expect_equal(adRpheno:::.parse_sex(c("m", "f")), c(0, 1))
  expect_equal(adRpheno:::.parse_sex(c("M", "F")), c(0, 1))
})

test_that(".parse_sex handles full-word encodings", {
  expect_equal(adRpheno:::.parse_sex(c("male", "female")), c(0, 1))
  expect_equal(adRpheno:::.parse_sex(c("Male", "Female")), c(0, 1))
  expect_equal(adRpheno:::.parse_sex(c("MALE", "FEMALE")), c(0, 1))
})

test_that(".parse_sex preserves NAs", {
  expect_true(is.na(adRpheno:::.parse_sex(NA)))
  expect_equal(adRpheno:::.parse_sex(c(0, NA, 1)), c(0, NA, 1))
})

test_that(".parse_sex returns NA for unrecognized values", {
  expect_true(is.na(adRpheno:::.parse_sex("unknown")))
  expect_true(is.na(adRpheno:::.parse_sex("white")))    # the bug from 1.1.1
  expect_true(is.na(adRpheno:::.parse_sex("X")))
})

test_that(".parse_sex handles factors (the original 1.1.1 bug)", {
  # A factor with race levels was being silently coerced to integer values
  # (1, 2, 3, ...) and treated as sex. Verify it now returns NAs instead.
  race_factor <- factor(c("white", "black", "asian"))
  expect_true(all(is.na(adRpheno:::.parse_sex(race_factor))))
})

test_that(".parse_sex handles mixed input types", {
  input  <- c("M", "f", "Male", "1", "0", "junk", NA)
  output <- adRpheno:::.parse_sex(input)
  expect_equal(output, c(0, 1, 0, 1, 0, NA, NA))
})
