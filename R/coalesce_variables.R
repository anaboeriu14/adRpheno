
#' Coalesce Variables by Priority
#'
#' Combines related variables into single variables using priority order.
#' Takes the first non-NA value from each group of variables.
#' Can work with manually specified groups or automatically detect groups by pattern.
#'
#' @param dataf A data frame
#' @param var_groups Named list. Groups of variables to coalesce, where each element
#'                  is a character vector of variables in priority order (highest to lowest)
#' @param pattern_extract Character. Optional regex pattern to automatically group variables.
#'                       If provided, variables will be grouped by removing this pattern
#' @param pattern_replace Character. Optional regex pattern to determine output column names.
#'                       Used with pattern_extract to clean variable names
#' @param prefix Character. Prefix for output column names (default: "")
#' @param force Logical. If TRUE, bypasses the check for previous processing (default: FALSE)
#'
#' @return Data frame with coalesced variables added
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   zscore_r3_nba = c(0.1, 0.2, NA),
#'   zscore_r5_nba = c(NA, 0.3, 0.4),
#'   zscore_r3_nhl = c(0.5, 0.6, NA),
#'   zscore_r5_nhl = c(NA, NA, 0.7)
#' )
#'
#' # Method 1: Manual groups (r3 has priority over r5)
#' var_groups <- list(
#'   nba = c("zscore_r3_nba", "zscore_r5_nba"),
#'   nhl = c("zscore_r3_nhl", "zscore_r5_nhl")
#' )
#' coalesce_variables(data, var_groups, prefix = "zscore_")
#'
#' # Method 2: Automatic grouping by pattern
#' coalesce_variables(data,
#'                   pattern_extract = "r\\d+",
#'                   pattern_replace = "zscore_r\\d+_",
#'                   prefix = "zscore_")
#'
coalesce_variables <- function(dataf, var_groups = NULL, pattern_extract = NULL,
                                    pattern_replace = NULL, prefix = "", force = FALSE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("Input 'dataf' must be a data frame.")
  }

  # Automatically generate groups if pattern_extract is provided
  if (is.null(var_groups) && !is.null(pattern_extract)) {
    var_groups <- list()

    # Get all potential variables
    all_vars <- colnames(dataf)

    # Group variables by extracting their base name without the pattern
    for (var in all_vars) {
      # Skip variables that don't match the pattern
      if (!grepl(pattern_extract, var)) next

      # Extract group identifier
      group_id <- stringr::str_extract(var, pattern_extract)

      # Extract base name without pattern
      if (!is.null(pattern_replace)) {
        base_name <- stringr::str_replace(var, pattern_replace, "")
      } else {
        # Default to removing the matched pattern
        base_name <- stringr::str_remove(var, group_id)
      }

      # Initialize group if needed
      if (!base_name %in% names(var_groups)) {
        var_groups[[base_name]] <- character()
      }

      # Add variable to group
      var_groups[[base_name]] <- c(var_groups[[base_name]], var)
    }

    # Sort variables within each group
    for (base_name in names(var_groups)) {
      # Extract group identifiers and sort numerically if possible
      group_ids <- sapply(var_groups[[base_name]], function(v) {
        stringr::str_extract(v, pattern_extract)
      })

      # Try to extract numeric part for sorting
      numeric_ids <- as.numeric(stringr::str_extract(group_ids, "\\d+"))
      if (!any(is.na(numeric_ids))) {
        # Sort by numeric IDs
        var_groups[[base_name]] <- var_groups[[base_name]][order(numeric_ids)]
      }
    }
  }

  # Validate var_groups
  if (is.null(var_groups)) {
    stop("Either var_groups or pattern_extract must be provided.")
  }

  # Check if specified variables exist
  all_vars <- unlist(var_groups)
  missing_vars <- all_vars[!all_vars %in% colnames(dataf)]
  if (length(missing_vars) > 0) {
    stop("The following variables do not exist in the data: ",
         paste(missing_vars, collapse = ", "))
  }

  # Check if these variables have already been processed
  if (!force) {
    adRutils::is_processed("merge_related_variables", all_vars, error_if_exists = TRUE)
  }

  # Create a copy of the data frame to modify
  result_df <- dataf

  # Process each group of variables
  for (base_name in names(var_groups)) {
    # Skip if fewer than 2 variables
    if (length(var_groups[[base_name]]) < 2) {
      warning("Skipping group '", base_name, "' with fewer than 2 variables.")
      next
    }

    # New column name for combined data
    new_col_name <- paste0(prefix, base_name)

    # Check for column name collisions
    if (new_col_name %in% colnames(result_df)) {
      warning("Column '", new_col_name, "' already exists. It will be overwritten.")
    }

    # Get the variables for this group
    group_vars <- var_groups[[base_name]]

    # Combine variables with coalesce
    if (length(group_vars) == 2) {
      result_df[[new_col_name]] <- dplyr::coalesce(result_df[[group_vars[1]]],
                                                   result_df[[group_vars[2]]])
    } else {
      # For more than 2 variables, build a more complex coalesce call
      coalesce_expr <- paste0("dplyr::coalesce(",
                              paste(sapply(group_vars, function(var) {
                                paste0("result_df[['", var, "']]")
                              }), collapse = ", "), ")")
      result_df[[new_col_name]] <- eval(parse(text = coalesce_expr))
    }
  }

  # Register these variables as processed
  adRutils::register_processed("merge_related_variables", all_vars)

  return(result_df)
}
