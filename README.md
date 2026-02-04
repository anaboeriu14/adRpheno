# adRpheno

[![Version](https://img.shields.io/badge/version-1.1.0-blue.svg)](https://github.com/anaboeriu14/adRpheno/releases) [![adRutils](https://img.shields.io/badge/adRutils-≥1.0.0-blue.svg)](https://github.com/anaboeriu14/adRutils) [![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)

> ## Tools for Alzheimer's Disease Endophenotype Analysis

## Overview

`adRpheno` provides a comprehensive suite of functions for processing and analyzing Alzheimer's Disease (AD) research data. The package streamlines common workflows including biomarker standardization, clinical calculations, cognitive assessments, genetic validation, and medication classification.

**Key Features:**

-   🧬 *APOE* genotype validation and risk classification

-   💊 RxNorm-based medication processing with intelligent caching

-   🧠 Cognitive composite scoring with demographic adjustments

-   📊 Clinical metrics (eGFR, BMI, blood pressure)

-   📈 Biomarker outlier detection and Z-score standardization

All functions include built-in caching, progress tracking, and comprehensive error handling for production use.

## Installation

Install from GitHub using `remotes`:

``` r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
install.packages("remotes")
}

# Install adRpheno
remotes::install_github("anaboeriu14/adRpheno")
```

## Quick Start

``` r
library(adRpheno)

# Add RxCUI codes to medications
meds_with_rxcui <- add_rxcuis(
  medications_df,
  med_column = "medication_name"
)

# Add ATC therapeutic classifications
meds_classified <- add_atc_classification(
  meds_with_rxcui,
  atc_level = "second"  # therapeutic subgroups
)

# Validate APOE genotypes
apoe_validated <- validate_apoe_e4_status(
  genetic_data,
  genotype = "apoe_genotype",
  e4_positive = "is_e4_carrier"
)

# Calculate clinical metrics
clinical_data <- calculate_egfr(
  patient_data,
  creatinine_col = "serum_creatinine",
  age_col = "age",
  sex_col = "sex"
)

# Create cognitive composites
cognitive_scores <- create_adjusted_composites(
  test_data,
  test_groups = list(
    memory = c("LM1", "LM2", "SEVLT_imm"),
    executive = c("digit_span", "trails_a")
  ),
  grouping_vars = c("age_group", "edu_group")
)
```

## Core Functions

### Medication Processing

Process and classify medications using RxNorm and WHO ATC systems with intelligent caching.

### APOE Genetics

Validate genotypes, verify e4 carrier status, and classify risk groups.

### Clinical Calculations

Calculate eGFR, BMI, blood pressure metrics, and other clinical measures.

### Cognitive Assessment

Create demographic-adjusted composites and standardize test scores.

### Data Processing

Handle outliers, standardize variables, and transform data structures.

See function documentation (`?function_name`) for complete details and examples.

## Performance Features

-   **Intelligent Caching**: Automatic caching of API results with configurable expiration
-   **Batch Processing**: Efficient processing of large datasets with progress tracking
-   **Optimized Defaults**: Pre-configured parameters for common use cases
-   **Retry Logic**: Automatic retry with exponential backoff for API failures

## Documentation

-   [NEWS.md](NEWS.md) - Version history and changelog
-   [Releases](https://github.com/anaboeriu14/adRpheno/releases) - Release notes

## Dependencies

**Core:**

-   [adRutils](https://github.com/anaboeriu14/adRutils) (≥ 1.1.0) - Utility functions and caching

-   [rxnorm](https://github.com/nt-williams/rxnorm) - RxNorm API interface

**Standard:**

-   dplyr, tidyr, stringr, purrr - Data manipulation

-   cli - User interface and progress tracking

-   httr, jsonlite - API interactions

See `DESCRIPTION` for complete dependency list.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Citation

If you use this package in your research, please cite:

```         
Boeriu, A. (2026). adRpheno: Tools for Alzheimer's Disease Endophenotype Analysis.
R package version 1.1.0. https://github.com/anaboeriu14/adRpheno
```

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Contact

Ana Boeriu - [GitHub](https://github.com/anaboeriu14)

## Acknowledgments

-   RxNorm API provided by the U.S. National Library of Medicine
-   WHO ATC classification system
