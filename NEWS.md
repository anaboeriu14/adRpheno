# adRpheno 0.3.0

## New Features

* Added `fit_group_models()` function for fitting linear models across multiple outcomes and groups
  - Supports flexible covariate specifications with outcome-specific and group-specific adjustments
  - Includes input validation using adRutils
  - Returns tidy results with model objects, summaries, and diagnostics
  - Useful for biomarker, cognitive, and neuroimaging analyses across ancestry groups

## Changes

* Created internal helper functions for better maintainability

## Breaking Changes

* None in this release

# adRpheno 0.2.0

## New Features

### Medication Analysis
- `add_rxcuis()` - Get RxCUI identifiers for medication names using RxNorm API
- `add_atc2_classification()` - Add ATC2 therapeutic classifications to medications
- Implemented caching system for API calls with expiration
- Batch processing with retry logic for failed API calls
- Support for both exact and approximate medication name matching

## Improvements
- Enhanced documentation across all functions
- Better error handling and validation

## Changes
- Updated license from MIT to GPL-3 to comply with rxnorm package dependencies

## Breaking Changes
- None 

# adRpheno 0.1.0

## Initial Release
* Biomarker Processing: Outlier detection, standardization, and merging
* Clinical Calculations: eGFR, BMI, and blood pressure metrics
* Cognitive Assessment: Demographic-adjusted scores and test component summation
* Genetic Data: APOE genotype validation, SNP matching, and risk classification

## Dependencies
* Requires adRutils >= 0.1.1
