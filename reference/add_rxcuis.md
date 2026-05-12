# Add RxCUI codes to medication dataframe

Looks up RxCUI codes for a column of medication names. Results are
cached to disk so reruns and overlapping queries are cheap. Medication
names should be pre-cleaned (ingredient names, no dosages) for best
match rates.

## Usage

``` r
add_rxcuis(
  dataf,
  med_column,
  rxcui_column = "rxcui",
  batch_size = 500,
  save_freq = 500,
  cache_dir = "cache",
  max_age_days = 30,
  retry_count = 3,
  batch_delay = 0.5
)
```

## Arguments

- dataf:

  Data frame with medication names.

- med_column:

  Column containing medication names.

- rxcui_column:

  Name for the new RxCUI column (default: `"rxcui"`). Overwritten with a
  warning if it already exists.

- batch_size:

  Medications per batch (default: 500).

- save_freq:

  Save cache every N items (default: 500).

- cache_dir:

  Cache directory (default: `"cache"`).

- max_age_days:

  Cache expiration in days (default: 30).

- retry_count:

  Retry attempts (default: 3).

- batch_delay:

  Seconds between batches (default: 0.5).

## Value

The input data frame with `rxcui_column` added.

## Examples

``` r
if (FALSE) { # \dontrun{
meds <- data.frame(med_clean = c("atorvastatin", "metformin", "lisinopril"))
add_rxcuis(meds, med_column = "med_clean")
} # }
```
