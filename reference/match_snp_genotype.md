# Match APOE SNPs to genotypes

Validates that SNP combinations (rs7412, rs429358) match reported
genotypes.

## Usage

``` r
match_snp_genotype(
  dataf,
  rs7412_col,
  rs429358_col,
  genotype_col,
  return_all = FALSE
)
```

## Arguments

- dataf:

  Data frame containing SNPs and genotypes

- rs7412_col:

  Character. Name of the rs7412 SNP column

- rs429358_col:

  Character. Name of the rs429358 SNP column

- genotype_col:

  Character. Name of the genotype column

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
mismatches <- match_snp_genotype(df, "rs7412", "rs429358", "apoe_genotype")
} # }
```
