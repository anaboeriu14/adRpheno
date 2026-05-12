# Pivot medication data from long to wide format

Transforms medication data from long format (one row per medication) to
wide format (one row per participant) while preserving medication
positions and aggregating category indicators.

## Usage

``` r
pivot_medication_data_wide(
  dataf,
  id_col,
  medication_col,
  position_col,
  category_cols = NULL,
  max_meds = NULL,
  fill_value = NA,
  med_prefix = "new_"
)
```

## Arguments

- dataf:

  Long-format data frame.

- id_col:

  Participant ID column.

- medication_col:

  Medication name column.

- position_col:

  Column whose values encode the position of each medication (e.g.
  `"medication_1_name"`, `"medication_2_name"`). The leading integer is
  extracted to determine the order. **Required; no default.**

- category_cols:

  Optional binary indicator columns to aggregate. If supplied, also
  produces `total_*_meds` (count per participant) and `taking_*_med`
  (0/1 indicator) summary columns.

- max_meds:

  Maximum medications per participant (default: `NULL`, meaning all).

- fill_value:

  Value for missing medication slots (default: `NA`).

- med_prefix:

  Prefix for medication columns (default: `"new_"`).

## Value

Wide-format data frame with one row per participant.

## Details

Summary column names assume `category_cols` follow the `is_<name>_med`
convention produced by
[`categorize_drugs()`](https://anaboeriu14.github.io/adRpheno/reference/categorize_drugs.md).
For input `is_diabetes_med`, the function generates
`total_diabetes_meds` and `taking_diabetes_med`. Columns not matching
this pattern still work but produce summary names that retain the
original prefixes/suffixes.

## Examples

``` r
if (FALSE) { # \dontrun{
wide_df <- pivot_medication_data_wide(
  long_df,
  id_col         = "participant_id",
  medication_col = "med_name",
  position_col   = "med_slot",
  category_cols  = c("is_diabetes_med", "is_hypertension_med")
)
} # }
```
