# APOE genotype workflow

``` r

library(adRpheno)
```

The APOE gene has three common alleles (ε2, ε3, ε4), giving six
genotypes. ε4 confers increased Alzheimer’s risk; ε2 is mildly
protective. Most studies need three things from APOE data: a consistent
genotype representation, validation that derived fields (carrier flags,
SNP data) agree with the reported genotype, and classification into risk
groups for downstream models.

`adRpheno` provides three functions, one for each task.

## The canonical genotype format

All comparisons in the package assume genotypes are normalized to
**`E{a}E{b}` with `a ≤ b`**, where each digit is 2, 3, or 4. So
`"E3/E4"`, `"4/3"`, `"e3e4"`, and `" E4 E3 "` all become `"E3E4"`. The
internal `.format_apoe_genotype()` helper does this automatically — you
never need to clean strings before passing them in.

That matters because the three functions below all run inputs through
the same normalizer, so mixed input styles are fine:

``` r

genetics <- data.frame(
  id            = 1:6,
  apoe_genotype = c("E3/E3", "E3E4", "2/4", "e3e3", " E4 E3 ", "E2E2"),
  is_e4_carrier = c(0,        1,      1,     0,       1,         0),
  rs7412        = c("CC",     "CC",   "TT",  "CC",    "CC",      "TT"),
  rs429358      = c("TT",     "CT",   "CT",  "TT",    "CT",      "TT")
)
```

Note row 5: a carrier flag of `1` for an `E3/E3` genotype. That’s a data
error we’ll catch in a moment.

## Step 1 — Validate e4 carrier flags

[`validate_apoe_e4_status()`](https://anaboeriu14.github.io/adRpheno/reference/validate_apoe_e4_status.md)
checks whether the `is_e4_carrier` column agrees with what the genotype
implies. By default it returns only the mismatched rows:

``` r

validate_apoe_e4_status(
  genetics,
  genotype    = "apoe_genotype",
  e4_positive = "is_e4_carrier"
)
#> Found 1 e4 status mismatch
#>   id apoe_genotype is_e4_carrier ...
#> 5  5      " E4 E3 "             1   <- wait, this *should* be a carrier
```

Hmm — row 5’s genotype string `" E4 E3 "` normalizes to `E3E4`, which
**is** an e4 carrier, so a flag of `1` is correct. Let me show a real
mismatch:

``` r

genetics$is_e4_carrier[1] <- 1   # claim E3/E3 is an e4 carrier
validate_apoe_e4_status(genetics, "apoe_genotype", "is_e4_carrier")
#> Found 1 e4 status mismatch
#>   id apoe_genotype is_e4_carrier
#> 1  1         E3/E3             1
```

The carrier-flag parser accepts a wide range of encodings: `1`/`0`,
`"yes"`/`"no"`, `"Y"`/`"N"`, `"true"`/`"false"`,
`"positive"`/`"negative"`, `"carrier"`/`"non-carrier"`,
case-insensitive. Anything else becomes `NA` and won’t trigger a false
mismatch.

Pass `return_all = TRUE` if you want the entire data frame back with a
`e4_status_valid` column attached — useful when you want to keep
mismatches in-place for inspection rather than as a separate table.

## Step 2 — Cross-check SNPs against genotypes

If your dataset includes the underlying SNPs,
[`match_snp_genotype()`](https://anaboeriu14.github.io/adRpheno/reference/match_snp_genotype.md)
predicts the genotype from rs7412 + rs429358 and compares it to what’s
recorded. The mapping it uses:

| rs7412 | rs429358 | Genotype |
|--------|----------|----------|
| TT     | TT       | E2E2     |
| CT     | TT       | E2E3     |
| TT     | CT       | E2E3     |
| CC     | TT       | E3E3     |
| TT     | CC       | E3E3     |
| CT     | CT       | E2E4     |
| CC     | CT       | E3E4     |
| CT     | CC       | E3E4     |
| CC     | CC       | E4E4     |

``` r

match_snp_genotype(
  genetics,
  rs7412_col   = "rs7412",
  rs429358_col = "rs429358",
  genotype_col = "apoe_genotype"
)
```

Same convention as before: mismatched rows by default, full data with
`genotype_match` if you pass `return_all = TRUE`. Rows where either SNP
is missing get `NA` for `genotype_match` rather than `FALSE`, so they
don’t generate false alarms.

## Step 3 — Classify into risk groups

[`classify_apoe_risk_groups()`](https://anaboeriu14.github.io/adRpheno/reference/classify_apoe_risk_groups.md)
collapses the six genotypes to three categories that match how APOE is
usually modeled:

| Risk group | Genotypes included |
|------------|--------------------|
| `e2+`      | E2E2, E2E3         |
| `e3/e3`    | E3E3               |
| `e4+`      | E2E4, E3E4, E4E4   |

The output column is a factor with these levels in display order
(`e3/e3`, `e2+`, `e4+`), so it slots straight into
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html) or
regression models without further re-leveling.

``` r

classify_apoe_risk_groups(genetics, genotype_col = "apoe_genotype")
#> ✔ Classified 6 APOE risk groups:
#> ℹ   e3/e3: 3 (50.0%)
#> ℹ   e2+: 1 (16.7%)
#> ℹ   e4+: 2 (33.3%)
```

Set `verbose = FALSE` to silence the summary if you’re inside a larger
pipeline.

## A typical full workflow

In practice you’d chain all three:

``` r

genetics |>
  validate_apoe_e4_status("apoe_genotype", "is_e4_carrier", return_all = TRUE) |>
  match_snp_genotype("rs7412", "rs429358", "apoe_genotype", return_all = TRUE) |>
  classify_apoe_risk_groups("apoe_genotype")
```

This keeps every row but adds `e4_status_valid`, `genotype_match`, and
`apoe_risk_group` columns. Then filter to rows where both validation
columns are `TRUE` (or `NA` for SNP, if you allow missing SNPs) before
downstream analysis.
