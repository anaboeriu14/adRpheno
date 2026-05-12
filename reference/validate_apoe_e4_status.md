# Validate APOE e4 carrier status

Checks for mismatches between APOE genotypes and e4 carrier status
flags.

## Usage

``` r
validate_apoe_e4_status(dataf, genotype, e4_positive, return_all = FALSE)
```

## Arguments

- dataf:

  Data frame containing APOE genotypes and e4 carrier status

- genotype:

  Character. Name of the APOE genotype column

- e4_positive:

  Character. Name of the e4 carrier status column

- return_all:

  Logical. Return entire dataset with validation column (default:
  `FALSE`)

## Value

When `return_all = FALSE`: a data frame of mismatch rows, or invisible
`NULL` if none. When `return_all = TRUE`: the full data frame with the
added validation column.

## Examples

``` r
if (FALSE) { # \dontrun{
# Mismatches only
mismatches <- validate_apoe_e4_status(df, "apoe_genotype", "is_e4_carrier")

# Full dataset with validation column
all_data <- validate_apoe_e4_status(df, "apoe_genotype", "is_e4_carrier",
                                    return_all = TRUE)
} # }
```
