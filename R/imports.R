#' @title adRpheno: Tools for Alzheimer's Disease Endophenotype Analysis
#'
#' @description
#' Provides functions for processing and analyzing Alzheimer's Disease (AD)
#' research data: APOE genotype validation, RxNorm-based medication
#' processing with caching, cognitive composite scoring, clinical metrics
#' (eGFR, BMI, blood pressure), and biomarker standardization.
#'
#' @name adRpheno
#'
#' @importFrom dplyr %>% across all_of case_when coalesce group_by if_else
#'   left_join mutate rename row_number select sym ungroup where
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_extract
#' @importFrom rlang .data :=
#'
#' @keywords internal
"_PACKAGE"
