# Standardize Numeric Variables

Creates standardized (z-score) versions of numeric variables. Z-scores
have mean = 0 and standard deviation = 1.

## Usage

``` r
compute_zscores(
  dataf,
  vars,
  prefix = "zscore_",
  group_vars = NULL,
  verbose = TRUE
)
```

## Arguments

- dataf:

  A data frame

- vars:

  Character vector of numeric variable names to standardize

- prefix:

  Prefix for standardized variable names (default: `"zscore_"`)

- group_vars:

  Optional character vector of grouping variables for group-wise
  standardization

- verbose:

  Show informative messages (default: `TRUE`)

## Value

Data frame with added standardized variable columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple standardization
result <- compute_zscores(df, vars = c("age", "weight", "height"))

# Group-wise standardization
result <- compute_zscores(
  df,
  vars       = c("test_score1", "test_score2"),
  group_vars = c("age_group", "sex")
)
} # }
```
