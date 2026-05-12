# Classify APOE genotypes into risk groups

Creates risk categories: e2+ (protective), e3/e3 (reference), e4+
(increased risk). If `group_col` already exists in `dataf`, it is
overwritten with a warning.

## Usage

``` r
classify_apoe_risk_groups(
  dataf,
  genotype_col,
  group_col = "apoe_risk_group",
  verbose = TRUE
)
```

## Arguments

- dataf:

  Data frame containing APOE genotype data

- genotype_col:

  Character. Name of the genotype column

- group_col:

  Character. Name for new risk group column (default:
  `"apoe_risk_group"`)

- verbose:

  Logical. Show informative messages (default: `TRUE`)

## Value

Data frame with added risk group column (factor)

## Examples

``` r
if (FALSE) { # \dontrun{
df_risk <- classify_apoe_risk_groups(df, "apoe_genotype")
} # }
```
