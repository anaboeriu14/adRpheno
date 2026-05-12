# Calculate Total Score from Multiple Test Components

Combines multiple test component scores using sum or mean.

## Usage

``` r
sum_cognitive_test_components(
  dataf,
  component_cols,
  result_col = "total_score",
  method = "sum",
  na.rm = TRUE,
  verbose = TRUE
)
```

## Arguments

- dataf:

  A data frame containing test component columns

- component_cols:

  Character vector of column names to combine

- result_col:

  Name for new total score column (default: `"total_score"`)

- method:

  Combination method: `"sum"` or `"mean"` (default: `"sum"`)

- na.rm:

  Remove `NA` values when combining (default: `TRUE`)

- verbose:

  Show informative messages (default: `TRUE`)

## Value

Data frame with added total score column

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sum_cognitive_test_components(
  dataf          = my_data,
  component_cols = c("trails_a_time", "trails_b_time"),
  result_col     = "trails_total"
)
} # }
```
