# ---- compute_zscores (simple) ----------------------------------------------

test_that("compute_zscores produces zscore columns with default prefix", {
  df <- data.frame(x = 1:10, y = seq(2, 20, by = 2))
  result <- compute_zscores(df, vars = c("x", "y"), verbose = FALSE)

  expect_true(all(c("zscore_x", "zscore_y") %in% names(result)))
})

test_that("compute_zscores produces standardized output (mean ~0, sd ~1)", {
  set.seed(1)
  df <- data.frame(x = rnorm(100, mean = 50, sd = 10))
  result <- compute_zscores(df, vars = "x", verbose = FALSE)

  expect_lt(abs(mean(result$zscore_x)),     1e-8)
  expect_lt(abs(sd(result$zscore_x) - 1.0), 1e-8)
})

test_that("compute_zscores respects custom prefix", {
  df <- data.frame(x = 1:10)
  result <- compute_zscores(df, vars = "x", prefix = "z_", verbose = FALSE)

  expect_true("z_x" %in% names(result))
  expect_false("zscore_x" %in% names(result))
})

test_that("compute_zscores leaves original columns untouched", {
  df <- data.frame(x = 1:5, y = 6:10)
  result <- compute_zscores(df, vars = "x", verbose = FALSE)

  expect_equal(result$x, df$x)
  expect_equal(result$y, df$y)
})


# ---- compute_zscores (grouped) ---------------------------------------------

test_that("compute_zscores standardizes within groups when group_vars supplied", {
  set.seed(2)
  df <- data.frame(
    group = rep(c("a", "b"), each = 50),
    x     = c(rnorm(50, mean = 100, sd = 10),
              rnorm(50, mean = 50,  sd = 10))
  )

  result <- compute_zscores(df, vars = "x", group_vars = "group",
                            verbose = FALSE)

  # Within each group, mean of zscores should be ~0
  by_group <- split(result$zscore_x, result$group)
  expect_lt(abs(mean(by_group[["a"]])), 1e-8)
  expect_lt(abs(mean(by_group[["b"]])), 1e-8)
  expect_lt(abs(sd(by_group[["a"]]) - 1.0), 1e-8)
  expect_lt(abs(sd(by_group[["b"]]) - 1.0), 1e-8)
})

test_that("compute_zscores with multiple group_vars works", {
  set.seed(3)
  df <- data.frame(
    age_group = rep(c("y", "o"), times = 50),
    sex       = rep(c("m", "f"), each  = 50),
    x         = rnorm(100)
  )
  result <- compute_zscores(df, vars = "x",
                            group_vars = c("age_group", "sex"),
                            verbose = FALSE)
  expect_true("zscore_x" %in% names(result))
  expect_equal(nrow(result), nrow(df))
})

test_that("compute_zscores rejects non-numeric variables", {
  df <- data.frame(x = letters[1:5])
  expect_error(compute_zscores(df, vars = "x", verbose = FALSE))
})

test_that("compute_zscores rejects missing columns", {
  df <- data.frame(x = 1:5)
  expect_error(compute_zscores(df, vars = "missing_col", verbose = FALSE))
})
