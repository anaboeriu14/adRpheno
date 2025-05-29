
# adRpheno

[![Version](https://img.shields.io/badge/version-0.2.0-blue.svg)](https://github.com/anaboeriu14/adRpheno/releases) [![adRutils Dependency](https://img.shields.io/badge/adRutils-%3E%3D%200.1.1-blue.svg)](https://github.com/anaboeriu14/adRutils)

Tools for Alzheimer's Disease Endophenotype Analysis

## Overview

`adRpheno` provides specialized functions for standardizing and analyzing Alzheimer's Disease (AD) endophenotypes.
The package focuses on biomarker Z-score standardization, clinical measurement calculations, cognitive testing,
and genetic data verification

## Version Information

**Current version:** 0.1.0

See [Releases](https://github.com/anaboeriu14/adRpheno/releases) for detailed change log and release notes.

## Installation

You can install the `adRpheno` package from GitHub using one of these methods:

### Option 1: Using remotes (recommended for most users)

The `remotes` package is lightweight and focused solely on package installation:

``` r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
# Install adRpheno from GitHub
remotes::install_github("anaboeriu14/adRpheno")
```

### Option 2: Using devtools

The `devtools` package is more comprehensive than `remotes`, with additional tools for package development:

``` r
#install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools") 
}
# Install adRpheno from GitHub
devtools::install_github("anaboeriu14/adRpheno")
```

## Features 

This package provides specialized tools for Alzheimer's Disease endophenotype analysis:

All functions feature caching and progress tracking for efficient usage

### Biomarker Processing

-   `detect_outlier_thresholds()`: Calculate lower and upper bounds for outlier detection using IQR method
-   `replace_outliers_with_na()`: Identify outliers in numeric variables and replace them with NA
-   `z_score()`: Create standardized z-scores of variables with optional group-wise standardization
-   `coalesce_variables()`: Combine related variables from different time points or sources into single variables

### Clinical Calculations

-   `calculate_egfr()`: Calculate estimated Glomerular Filtration Rate (eGFR) using the CKD-EPI formula
-   `calculate_bmi_obesity()`: Calculate BMI and classify obesity status
-   `calculate_bp_metrics()`: Calculate blood pressure metrics including Mean Arterial Pressure (MAP) and Pulse Pressure

### Cognitive Assessment

-   `create_demographic_adjusted_composites()`: Compute z-scores adjusted for demographic factors and calculate composite scores
-   `sum_cognitive_test_components()`: Calculate total scores from multiple test components using sum or mean methods

### Genetic Data
-   `validate_apoe_e4_status()`: Validate APOE e4 carrier status against genotype
-   `match_snp_genotype()`: Match APOE SNPs (rs7412, rs429358) to expected genotypes
-   `classify_apoe_risk_groups()`: Classify APOE genotypes into risk groups (e2+, e3/e3, e4+)

### Medication Data Processing
- `add_rxcuis()` - Get RxCUI identifiers for medication names using RxNorm API with caching
- `add_atc2_classification()` - Add ATC2 therapeutic classifications to medications

## Dependencies

This package depends on: 
- [adRutils](https://github.com/anaboeriu14/adRutils) (>= 0.1.1)
- [rxnorm](https://github.com/nt-williams/rxnorm),

## License

This project is licensed under the GLP-3 License
