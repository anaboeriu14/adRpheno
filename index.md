# adRpheno

[![Version](https://img.shields.io/badge/version-2.0.0-blue.svg)](https://github.com/anaboeriu14/adRpheno/releases)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/anaboeriu14/adRpheno/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/anaboeriu14/adRpheno/actions/workflows/R-CMD-check.yaml)

Tools for processing and analyzing Alzheimer’s Disease (AD)
endophenotypes: APOE genotype validation, RxNorm-based medication
processing with on-disk caching, cognitive composite scoring, clinical
metrics (eGFR, BMI, blood pressure), and biomarker standardization.

> **Version 2.0.0 is a major cleanup release.** The package was
> refactored end-to-end: vendored RxClass API access (no more `rxnorm`
> dependency), `validate_args` migration, atomic cache writes, and a
> `testthat` suite. License switched from GPL-3 to MIT. See
> [NEWS.md](https://github.com/anaboeriu14/adRpheno/blob/main/NEWS.md)
> for the full list.

## Installation

`adRpheno` depends on `adRutils`, also installed from GitHub. The
`Remotes:` field in `DESCRIPTION` resolves this automatically.

### Option 1: Using remotes (recommended)

``` r

# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install adRpheno (and adRutils)
remotes::install_github("anaboeriu14/adRpheno")
```

### Option 2: Using devtools

``` r

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("anaboeriu14/adRpheno")
```

## Quick Start

``` r

library(adRpheno)

# Add RxCUI codes to medications, then ATC therapeutic classifications
meds <- medications_df %>%
  add_rxcuis(med_column = "medication_name") %>%
  add_atc_classification(atc_level = "second")

# Categorize by therapeutic class (cardiometabolic defaults)
meds <- categorize_drugs(meds, med_col = "medication_name", atc_col = "atc2_class")

# Validate APOE genotypes against e4 carrier status
mismatches <- validate_apoe_e4_status(
  genetic_data,
  genotype    = "apoe_genotype",
  e4_positive = "is_e4_carrier"
)

# CKD-EPI 2021 eGFR
clinical_data <- calculate_egfr(
  patient_data,
  creatinine_col = "serum_creatinine",
  age_col        = "age",
  sex_col        = "sex"
)

# Demographic-adjusted cognitive composites
cognitive_scores <- create_adjusted_composites(
  test_data,
  test_groups = list(
    memory    = c("LM1", "LM2", "SEVLT_imm"),
    executive = c("digit_span", "trails_a")
  ),
  grouping_vars = c("age_group", "edu_group")
)
```

## Documentation

For full documentation, including function details and examples:

``` r

# Browse all functions
help(package = "adRpheno")

# Function-specific help
?add_atc_classification
?validate_apoe_e4_status
?calculate_egfr
```

## Version Information

**Current version:** 2.0.0

See [Releases](https://github.com/anaboeriu14/adRpheno/releases) and
[NEWS.md](https://github.com/anaboeriu14/adRpheno/blob/main/NEWS.md) for
the full changelog.

## License

This project is licensed under the MIT License. Versions ≤ 1.1.1 were
released under GPL-3.

## Citation

If you use this package in your research, please cite:

    Boeriu, A. (2026). adRpheno: Tools for Alzheimer's Disease Endophenotype Analysis.
    R package version 2.0.0. https://github.com/anaboeriu14/adRpheno

## Contact

For questions or issues, please open an issue on
[GitHub](https://github.com/anaboeriu14/adRpheno/issues).

## Acknowledgments

- RxNorm and RxClass APIs provided by the U.S. National Library of
  Medicine
- WHO ATC/DDD classification system
