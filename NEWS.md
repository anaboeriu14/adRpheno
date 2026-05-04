# adRpheno 2.0.0

Major cleanup release. The package was refactored end-to-end for stability, professional polish, and reduced external risk. The function reference reflects the new state; the changes are summarized below. See git history for per-function detail.

## New

-   Vendored RxClass API access (`add_atc_classification()` now calls the National Library of Medicine RxClass REST API directly instead of via the unmaintained `rxnorm` GitHub package).
-   `add_atc_classification(as_list_column = TRUE)` returns a list-column instead of exploding rows; downstream callers can use `tidyr::unnest_longer()` if they want the previous shape.
-   `is_cache_expired()` for cache-level age checks.
-   Added a `tests/testthat/` suite covering APOE helpers and validation, clinical calculations, cognitive composites, drug categorization, and z-score standardization.

## Breaking changes

-   `rxnorm` GitHub package dependency dropped.
-   `add_atc_classification()`: `unnest = TRUE` replaced by `as_list_column = TRUE`.
-   `pivot_medication_data_wide()`: `position_col` is now required (no default).
-   `get_single_rxcui()`: lost its `retry_count` parameter. Retry now happens at the batch-orchestration layer only.
-   `reverse_cognitive_scores()` removed (was unused; multiply by `-1` directly if needed).
-   `outliers.R` (`detect_outlier_thresholds()`, `replace_outliers_with_na()`) moved to `adRutils` (it's domain-agnostic). Use `adRutils::detect_outlier_thresholds()` instead.
-   All public functions migrated to `adRutils::validate_args` with the new assertion helpers (`is_string`, `is_flag`, `is_one_of`, etc.).

## Fixes

-   `add_atc_classification()`: column-already-exists warning now fires for the auto-generated default name
    -   previously the check ran before name resolution, so the warning was silent).
-   Helper names and labels (`.apply_atc2_*`, `"ATC2:"`) no longer hardcode level 2; they reflect whichever `atc_level` was used.
-   `categorize_drugs()`: `prefix = ""` and `suffix = ""` are now accepted (previously rejected by validation).
-   `cache.R`: `save_cache()` writes atomically (temp-file + rename) and `initialize_cache()` validates the loaded object's shape and recovers with warnings from corruption.
-   Batch processor: removed double-retry between `get_single_rxcui()` and `.process_batch()`
    -   previously a single failure could trigger up to 9 attempts). Retry now uses exponential backoff (0.5s → 1s → 2s).
-   Vectorized `.format_apoe_genotype()` and `.predict_genotype_from_snps()` for performance and clarity.
    -   Fixed broken character class in genotype-cleaning regex.

## Other

-   Cache utilities moved from `adRutils` to `adRpheno` (closer to their actual users).
-   `imports.R` cleaned up: dropped duplicate `@import cli`, dropped unused imports, switched broad `@import dplyr` to explicit `@importFrom dplyr` directives. Replaced `NULL` with `"_PACKAGE"` sentinel for the package help page.
-   File renamed: `pivot_medication_data_wide.R` → `drug_pivot_wide.R` (matches the `drug_*.R` convention).

------------------------------------------------------------------------
# adRpheno 1.1.1

## Bug Fixes

-   Fixed `calculate_egfr()` silently accepting invalid sex columns (e.g., race factors) due to R's implicit factor-to-integer coercion, producing incorrect eGFR values
-   Added .parse_sex() helper to normalize sex encodings (0/1, m/f, male/female) with clear error messages for unrecognized values

## Changes

-   None

## Breaking Changes

-   

    ## None

# adRpheno 1.1.0

## New Features

-   Added `reverse_test_scores()` to cognitive_composites.R for reversing cognitive test scores where higher values indicate worse performance (e.g., Trail Making Test times)

## Changes

-   Enhanced documentation for cognitive scoring functions
-   Improved consistency in function naming and parameter validation

## Breaking Changes

-   None

## Bug Fixes

-   None

------------------------------------------------------------------------

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

-   Requires adRutils \>= 1.1.0
-   All other dependencies unchanged

------------------------------------------------------------------------

# adRpheno 0.1.0

## Initial Release

-   Biomarker Processing: Outlier detection, standardization, and merging
-   Clinical Calculations: eGFR, BMI, and blood pressure metrics
-   Cognitive Assessment: Demographic-adjusted scores and test component summation
-   Genetic Data: APOE genotype validation, SNP matching, and risk classification

## Dependencies

-   Requires adRutils \>= 0.1.1
