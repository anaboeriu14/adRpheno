# adRpheno 0.2.0

## New Features
* Added medication data processing functionality:
  * `add_medication_atc2()`: Get ATC2 therapeutic classifications for medications (with unnested option)
  * `get_rxnorm_medication_names()`: Standardize medication names via RxNorm API
  * `correct_medication_names_df()`: Apply RxNorm standardization to dataframe columns
  * `add_medication_rxcuis()`: Retrieve RxCUI identifiers for medications

## Improvements
* Implemented caching system for improved performance across all API functions
* Added progress bars for monitoring long-running operations
* Optimized processing of unique medication values to minimize API calls

## Changes
* Updated license to GPL-3 to comply with rxnorm package dependencies
* Added required dependencies: rxnorm, dplyr, httr, jsonlite
* Added tidyr as a suggested package

# adRpheno 0.1.0

## Initial Release
* Biomarker Processing: Outlier detection, standardization, and merging
* Clinical Calculations: eGFR, BMI, and blood pressure metrics
* Cognitive Assessment: Demographic-adjusted scores and test component summation
* Genetic Data: APOE genotype validation, SNP matching, and risk classification

## Dependencies
* Requires adRutils >= 0.1.1
