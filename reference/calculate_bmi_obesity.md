# Calculate BMI and classify obesity status

Calculates BMI using imperial units (weight in pounds, height in
inches), renames measurement columns for clarity, and adds obesity
classification.

## Usage

``` r
calculate_bmi_obesity(
  dataf,
  weight_col,
  height_col,
  bmi_col,
  cutoff = 30,
  verbose = TRUE
)
```

## Arguments

- dataf:

  A data frame containing the data

- weight_col:

  String. Column name containing weight in pounds (lbs)

- height_col:

  String. Column name containing height in inches

- bmi_col:

  String. Existing BMI column to preserve as `bmi_original`

- cutoff:

  Numeric. BMI cutoff value for obesity classification (default: 30)

- verbose:

  Logical. If `TRUE`, shows informative messages (default: `TRUE`)

## Value

Data frame with calculated BMI values and obesity classification. Note:
`weight_col` is renamed to `weight_lbs`, `height_col` to `height_inch`,
and `bmi_col` to `bmi_original`. New columns added: `bmi_imperial`,
`obesity`.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(weight = c(150, 180, 200),
                 height = c(65, 70, 72),
                 bmi    = c(25, 26, 27))
calculate_bmi_obesity(df, "weight", "height", "bmi")
} # }
```
