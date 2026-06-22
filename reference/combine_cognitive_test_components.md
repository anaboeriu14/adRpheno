# Combines multiple test component scores into one column using either a sum or a mean.

Combines multiple test component scores into one column using either a
sum or a mean.

## Usage

``` r
combine_cognitive_test_components(
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

  Name for new score column (default: `"total_score"`)

- method:

  Combination method: `"sum"` or `"mean"` (default: `"sum"`)

- na.rm:

  Remove `NA` values when combining (default: `TRUE`)

- verbose:

  Show informative messages (default: `TRUE`)

## Value

Data frame with the added combined-score column

## Examples

``` r
if (FALSE) { # \dontrun{
# Sum of two Trail Making Test times
result <- combine_cognitive_test_components(
  dataf          = my_data,
  component_cols = c("trails_a_time", "trails_b_time"),
  result_col     = "trails_total"
)

# Mean of several memory tests
result <- combine_cognitive_test_components(
  dataf          = my_data,
  component_cols = c("recall_1", "recall_2", "recognition"),
  result_col     = "memory_mean",
  method         = "mean"
)
} # }
```
