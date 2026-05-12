# Compute Adjusted Composite Scores

Computes z-scores for test scores adjusted for demographic factors and
calculates composite scores for specified test groups.

## Usage

``` r
create_adjusted_composites(
  dataf,
  test_groups,
  grouping_vars,
  filters = NULL,
  digits = 3,
  verbose = TRUE
)
```

## Arguments

- dataf:

  A data frame containing raw test scores and demographic information

- test_groups:

  Named list of character vectors, one per composite group. Each element
  names the columns to include in that group's composite.

- grouping_vars:

  Character vector of demographic grouping columns

- filters:

  Optional named list of string filter expressions evaluated against
  `dataf` (e.g. `list(diagnosis = "diagnosis == 'CN'")`)

- digits:

  Integer for decimal places (default: 3)

- verbose:

  Logical. Show informative messages (default: `TRUE`)

## Value

Data frame with added z-score columns (`zscore_{var}_{group}`) and
composite columns (`{group}_comp_score`).

## Examples

``` r
if (FALSE) { # \dontrun{
test_groups <- list(
  memory    = c("LM1", "LM2", "SEVLT_imm"),
  executive = c("digit_span", "trails_a")
)

result <- create_adjusted_composites(
  dataf         = my_data,
  test_groups   = test_groups,
  grouping_vars = c("age_group", "edu_group")
)
} # }
```
