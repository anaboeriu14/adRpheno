# Package index

## Medication processing

Look up RxCUI codes, attach WHO ATC classifications, categorize drugs by
therapeutic class, and reshape long medication data to wide format. All
API calls are cached to disk.

- [`get_single_rxcui()`](https://anaboeriu14.github.io/adRpheno/reference/get_single_rxcui.md)
  : Get RxCUI for a single medication
- [`add_rxcuis()`](https://anaboeriu14.github.io/adRpheno/reference/add_rxcuis.md)
  : Add RxCUI codes to medication dataframe
- [`add_atc_classification()`](https://anaboeriu14.github.io/adRpheno/reference/add_atc_classification.md)
  : Add ATC classifications to medications
- [`categorize_drugs()`](https://anaboeriu14.github.io/adRpheno/reference/categorize_drugs.md)
  : Categorize medications by therapeutic class
- [`pivot_medication_data_wide()`](https://anaboeriu14.github.io/adRpheno/reference/pivot_medication_data_wide.md)
  : Pivot medication data from long to wide format

## APOE genetics

Validate APOE genotypes against e4 carrier flags and rs7412/rs429358 SNP
data, and classify genotypes into risk groups.

- [`validate_apoe_e4_status()`](https://anaboeriu14.github.io/adRpheno/reference/validate_apoe_e4_status.md)
  : Validate APOE e4 carrier status
- [`match_snp_genotype()`](https://anaboeriu14.github.io/adRpheno/reference/match_snp_genotype.md)
  : Match APOE SNPs to genotypes
- [`classify_apoe_risk_groups()`](https://anaboeriu14.github.io/adRpheno/reference/classify_apoe_risk_groups.md)
  : Classify APOE genotypes into risk groups

## Clinical calculations

BMI with obesity classification, blood pressure metrics (mean arterial
pressure, pulse pressure), and CKD-EPI 2021 eGFR.

- [`calculate_bmi_obesity()`](https://anaboeriu14.github.io/adRpheno/reference/calculate_bmi_obesity.md)
  : Calculate BMI and classify obesity status
- [`calculate_bp_metrics()`](https://anaboeriu14.github.io/adRpheno/reference/calculate_bp_metrics.md)
  : Calculate blood pressure metrics
- [`calculate_egfr()`](https://anaboeriu14.github.io/adRpheno/reference/calculate_egfr.md)
  : Calculate estimated Glomerular Filtration Rate (eGFR)

## Cognitive composites & standardization

Compute z-scores (optionally group-wise) and build demographic-adjusted
cognitive composite scores from groups of test variables.

- [`compute_zscores()`](https://anaboeriu14.github.io/adRpheno/reference/compute_zscores.md)
  : Standardize Numeric Variables
- [`create_adjusted_composites()`](https://anaboeriu14.github.io/adRpheno/reference/create_adjusted_composites.md)
  : Compute Adjusted Composite Scores
- [`sum_cognitive_test_components()`](https://anaboeriu14.github.io/adRpheno/reference/sum_cognitive_test_components.md)
  : Calculate Total Score from Multiple Test Components
