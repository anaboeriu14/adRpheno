#' Add ATC2 therapeutic classifications to medications
#'
#' This function takes a data frame with medication information (including RxCUIs)
#' and adds a new column with ATC2 therapeutic classifications.
#'
#' @param dataf A data frame containing medication information
#' @param rxcui_col Name of the column containing RxCUI values (default: "rxcui")
#' @param new_col_name Name of the new column to create (default: "atc2_class")
#' @param unnest Logical. If TRUE, returns one row per ATC2 class (default: FALSE)
#' @param ... Additional arguments passed to the processing functions
#'
#' @return A data frame with an additional column containing ATC2 classifications.
#' @export
add_atc2_classification <- function(dataf, rxcui_col = "rxcui", new_col_name = "atc2_class",
                                    unnest = TRUE, ...) {

  if (unnest) {
    add_medication_atc2_unnested(dataf, rxcui_col, new_col_name, ...)
  } else {
    add_medication_atc2_nested(dataf, rxcui_col, new_col_name, ...)
  }
}
