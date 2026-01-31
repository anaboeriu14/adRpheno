# adRpheno 1.0.0

## Stable Release (2026-01-30)

First stable release with production-ready features, optimized performance, and enhanced caching.

### Breaking Changes

None - all existing code remains compatible.

### New Features

-   `add_atc_classification()` - Flexible ATC classification with support for all levels (first/second/third/fourth)
-   `categorize_drugs()` - Pattern-based medication categorization with binary indicators
-   `pivot_medication_data_wide()` - Transform long medication data to wide format
-   Auto-generated column names based on parameters (e.g., `atc2_class` for level "second")

### Performance Improvements

-   **faster** processing for larger datasets
-   Optimized defaults: `batch_size=500`, `batch_delay=0.5s`, `save_freq=500`
-   Level-specific cache files prevent collisions (`atc1_cache`, `atc2_cache`, etc.)
-   Enhanced progress bars show both cached and new items

### Bug Fixes

-   Fixed cache key collision when using different ATC levels
-   Fixed `add_rxcuis()` returning list instead of character vector
-   Fixed save frequency calculation (now batch-based, not item-based)
-   Fixed progress bar showing 0% when most items are cached

## Changes

-   Updated license from MIT to GPL-3 to comply with rxnorm package dependencies

### Improvements

-   Better progress tracking
-   Cleaner anonymous functions replace wrapper functions
-   Guaranteed character vector output using `vapply`
-   More informative cache statistics and completion messages
-   Streamlined documentation with practical examples

### Dependencies

-   Requires adRutils >= 1.1.0
-   All other dependencies unchanged

------------------------------------------------------------------------


# adRpheno 0.1.0

## Initial Release

-   Biomarker Processing: Outlier detection, standardization, and merging
-   Clinical Calculations: eGFR, BMI, and blood pressure metrics
-   Cognitive Assessment: Demographic-adjusted scores and test component summation
-   Genetic Data: APOE genotype validation, SNP matching, and risk classification

## Dependencies

-   Requires adRutils >= 0.1.1
