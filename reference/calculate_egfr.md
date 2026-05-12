# Calculate estimated Glomerular Filtration Rate (eGFR)

Calculates eGFR using the CKD-EPI 2021 formula based on serum
creatinine, age, and sex.

## Usage

``` r
calculate_egfr(
  dataf,
  creatinine_col,
  age_col,
  sex_col,
  egfr_col = "eGFR",
  verbose = TRUE
)
```

## Arguments

- dataf:

  A data frame containing the input data

- creatinine_col:

  String. Column name for serum creatinine in mg/dL

- age_col:

  String. Column name for age in years

- sex_col:

  String. Column name for sex. Accepts 0/1, "m"/"f", "male"/"female"
  (case-insensitive). 1/female = female; 0/male = male.

- egfr_col:

  String. Output column name (default: `"eGFR"`)

- verbose:

  Logical. If `TRUE`, shows informative messages (default: `TRUE`)

## Value

Data frame with added eGFR column

## Details

Uses the CKD-EPI (Chronic Kidney Disease Epidemiology Collaboration)
2021 equation:
`eGFR = 142 * min(SCr/k, 1)^a * max(SCr/k, 1)^-1.200 * 0.9938^age * S`
where for females `k = 0.7, a = -0.241, S = 1.012`, and for males
`k = 0.9, a = -0.302, S = 1`.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(creatinine = c(1.2, 0.8), age = c(50, 60), sex = c(0, 1))
calculate_egfr(df, "creatinine", "age", "sex")
} # }
```
