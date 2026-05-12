# Add ATC classifications to medications

Retrieves WHO ATC classifications for medications using RxCUI codes via
direct calls to the National Library of Medicine RxClass REST API.
Results are cached to disk so reruns and overlapping queries are cheap.

## Usage

``` r
add_atc_classification(
  dataf,
  rxcui_col = "rxcui",
  new_col_name = NULL,
  as_list_column = FALSE,
  atc_level = "second",
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

  Data frame containing RxCUI values.

- rxcui_col:

  Column with RxCUI values (default: `"rxcui"`).

- new_col_name:

  Name for the new column. If `NULL` (default), an auto-generated name
  based on `atc_level` is used (e.g. `"atc2_class"`).

- as_list_column:

  Logical. If `TRUE`, the output column is a list-column where each
  entry is a character vector of ATC classes for that drug. If `FALSE`
  (default), multiple classes are joined by `"; "`.

- atc_level:

  ATC level: `"first"`, `"second"` (default), `"third"`, or `"fourth"`.

- batch_size:

  Items per batch (default: 500).

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

The input data frame with the ATC classification column added.

## Details

For drugs with multiple ATC classifications, the default behavior
(`as_list_column = FALSE`) collapses them into a single
semicolon-separated string. Setting `as_list_column = TRUE` instead
stores the per-row classifications as a list-column, which preserves
multi-value structure for downstream tidyverse work (e.g.
[`tidyr::unnest_longer()`](https://tidyr.tidyverse.org/reference/unnest_longer.html)).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(med   = c("atorvastatin", "metformin"),
                 rxcui = c("83367", "6809"))

# Default: collapsed string
add_atc_classification(df)

# List-column for tidyverse-style downstream work
add_atc_classification(df, as_list_column = TRUE)

# Different level
add_atc_classification(df, atc_level = "third")
} # }
```
