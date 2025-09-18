
# adRpheno

[![Version](https://img.shields.io/badge/version-0.3.0-blue.svg)](https://github.com/anaboeriu14/adRpheno/releases) [![adRutils Dependency](https://img.shields.io/badge/adRutils-0.4.0-blue.svg)](https://github.com/anaboeriu14/adRutils)

Tools for Alzheimer's Disease Endophenotype Analysis

## Overview

`adRpheno` provides specialized functions for standardizing and analyzing Alzheimer's Disease (AD) endophenotypes.
The package focuses on biomarker Z-score standardization, clinical measurement calculations, cognitive testing,
and genetic data verification

See [NEWS.md](NEWS.md) for detailed changelong and [Releases](https://github.com/anaboeriu14/adRpheno/releases) for release notes.

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

- **Biomarker Processing**: Outlier detection, standardization, and variable merging
- **Clinical Calculations**: eGFR, BMI, blood pressure, and other clinical metrics  
- **Cognitive Assessment**: Demographic adjustments and composite scoring
- **Genetic Data**: APOE genotype validation and risk classification
- **Medication Processing**: RxNorm integration, therapeutic classification, and data transformation

All functions feature caching and progress tracking for efficient usage. 
See function documentation for complete details on available methods.

## Dependencies

This package depends on: 
- [adRutils](https://github.com/anaboeriu14/adRutils) (>= 0.1.1)
- [rxnorm](https://github.com/nt-williams/rxnorm),

## License

This project is licensed under the GLP-3 License
