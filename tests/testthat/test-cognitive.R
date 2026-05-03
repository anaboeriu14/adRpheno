# ---- sum_cognitive_test_components -----------------------------------------

test_that("sum_cognitive_test_components defaults to row sums", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  result <- sum_cognitive_test_components(df, c("a", "b"),
                                          result_col = "total",
                                          verbose = FALSE)
  expect_equal(result$total, c(5, 7, 9))
})

test_that("sum_cognitive_test_components computes row means when method='mean'", {
  df <- data.frame(a = c(1, 2, 3), b = c(3, 4, 5))
  result <- sum_cognitive_test_components(df, c("a", "b"),
                                          result_col = "avg",
                                          method = "mean",
                                          verbose = FALSE)
  expect_equal(result$avg, c(2, 3, 4))
})

test_that("sum_cognitive_test_components handles NAs based on na.rm", {
  df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))

  # na.rm = TRUE (default): missing values treated as zero in sum
  r_keep <- sum_cognitive_test_components(df, c("a", "b"),
                                          result_col = "total",
                                          na.rm = TRUE, verbose = FALSE)
  expect_equal(r_keep$total, c(5, 5, 3))

  # na.rm = FALSE: any NA propagates
  r_drop <- sum_cognitive_test_components(df, c("a", "b"),
                                          result_col = "total",
                                          na.rm = FALSE, verbose = FALSE)
  expect_equal(r_drop$total, c(5, NA, NA))
})

test_that("sum_cognitive_test_components rejects invalid method", {
  df <- data.frame(a = 1, b = 2)
  expect_error(
    sum_cognitive_test_components(df, c("a", "b"), method = "median",
                                  verbose = FALSE)
  )
})


# ---- create_adjusted_composites --------------------------------------------

test_that("create_adjusted_composites builds expected zscore and composite columns", {
  set.seed(42)
  df <- data.frame(
    age_group = rep(c("young", "old"), each = 10),
    LM1       = rnorm(20),
    LM2       = rnorm(20),
    DSF       = rnorm(20),
    DSB       = rnorm(20)
  )

  test_groups <- list(
    memory    = c("LM1", "LM2"),
    executive = c("DSF", "DSB")
  )

  result <- create_adjusted_composites(df, test_groups,
                                       grouping_vars = "age_group",
                                       verbose = FALSE)

  # Expected new columns
  expect_true("zscore_LM1_memory"     %in% names(result))
  expect_true("zscore_LM2_memory"     %in% names(result))
  expect_true("zscore_DSF_executive"  %in% names(result))
  expect_true("zscore_DSB_executive"  %in% names(result))
  expect_true("memory_comp_score"     %in% names(result))
  expect_true("executive_comp_score"  %in% names(result))
})

test_that("create_adjusted_composites errors on missing test columns", {
  df <- data.frame(age_group = "young", LM1 = 1.0)
  expect_error(
    create_adjusted_composites(df,
                               test_groups   = list(memory = c("LM1", "LM2")),
                               grouping_vars = "age_group",
                               verbose = FALSE),
    "Missing test column"
  )
})

test_that("create_adjusted_composites errors on non-list test_groups", {
  df <- data.frame(age_group = "young", LM1 = 1.0)
  expect_error(
    create_adjusted_composites(df,
                               test_groups   = c("LM1"),
                               grouping_vars = "age_group",
                               verbose = FALSE),
    "named list"
  )
})

test_that("create_adjusted_composites computes zscores grouped by demographics", {
  # Within each age group, the mean of zscores should be ~0
  set.seed(123)
  df <- data.frame(
    age_group = rep(c("a", "b"), each = 50),
    score     = c(rnorm(50, mean = 100, sd = 10),
                  rnorm(50, mean = 50,  sd = 10))
  )

  result <- create_adjusted_composites(
    df, test_groups = list(s = "score"), grouping_vars = "age_group",
    verbose = FALSE
  )

  # Check that within each group, the zscores have mean close to 0
  by_group <- split(result$zscore_score_s, result$age_group)
  expect_lt(abs(mean(by_group[["a"]])), 0.01)
  expect_lt(abs(mean(by_group[["b"]])), 0.01)
})
