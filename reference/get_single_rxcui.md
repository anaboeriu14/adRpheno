# Get RxCUI for a single medication

Queries the RxNorm API using "exact then normalized" matching, which
handles abbreviations (e.g. "hctz" -\> "hydrochlorothiazide") without
false positives.

## Usage

``` r
get_single_rxcui(med_name, timeout = 10)
```

## Arguments

- med_name:

  Medication name (ideally an ingredient name).

- timeout:

  API timeout in seconds (default: 10).

## Value

RxCUI as a character scalar, or `NA_character_` if the lookup fails or
returns no match.

## Details

Note: this is a thin single-call wrapper with no caching. For batch
lookups, use
[`add_rxcuis()`](https://anaboeriu14.github.io/adRpheno/reference/add_rxcuis.md),
which caches results to disk.

## Examples

``` r
if (FALSE) { # \dontrun{
get_single_rxcui("atorvastatin")
get_single_rxcui("hctz")
} # }
```
