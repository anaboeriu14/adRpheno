#' Validate inputs for add_atc2_classification function
#' @noRd
.validate_atc2_inputs <- function(dataf, rxcui_col, new_col_name, unnest,
                                  batch_size, save_freq, cache_dir, max_age_days,
                                  retry_count, batch_delay) {

  adRutils::validate_params(
    data = dataf,
    columns = rxcui_col,
    custom_checks = list(
      list(
        condition = is.character(new_col_name) && length(new_col_name) == 1 &&
          !is.na(new_col_name) && new_col_name != "",
        message = "new_col_name must be a single non-empty character string"
      ),
      # unnest validation
      list(
        condition = is.logical(unnest) && length(unnest) == 1 && !is.na(unnest),
        message = "unnest must be TRUE or FALSE"
      ),
      # batch_size validation
      list(
        condition = is.numeric(batch_size) && length(batch_size) == 1 &&
          batch_size > 0 && batch_size <= 1000,
        message = "batch_size must be a single number between 1 and 1000"
      ),
      # save_freq validation
      list(
        condition = is.numeric(save_freq) && length(save_freq) == 1 && save_freq > 0,
        message = "save_freq must be a single positive number"
      ),
      # cache_dir validation
      list(
        condition = is.character(cache_dir) && length(cache_dir) == 1 && cache_dir != "",
        message = "cache_dir must be a single non-empty character string"
      ),
      # max_age_days validation
      list(
        condition = is.numeric(max_age_days) && length(max_age_days) == 1 && max_age_days > 0,
        message = "max_age_days must be a single positive number"
      ),
      # retry_count validation
      list(
        condition = is.numeric(retry_count) && length(retry_count) == 1 &&
          retry_count >= 1 && retry_count <= 5,
        message = "retry_count must be a single number between 1 and 5"
      ),
      # batch_delay validation
      list(
        condition = is.numeric(batch_delay) && length(batch_delay) == 1 && batch_delay >= 0,
        message = "batch_delay must be a single non-negative number"
      ),
      # Data frame has rows
      list(
        condition = nrow(dataf) > 0,
        message = "Input data frame is empty (0 rows)"
      ),
      # Has some valid RxCUIs
      list(
        condition = {
          rxcui_values <- dataf[[rxcui_col]]
          non_na_rxcuis <- rxcui_values[!is.na(rxcui_values)]
          length(non_na_rxcuis) > 0
        },
        message = paste0("All values in column '", rxcui_col, "' are NA. Cannot process ATC2 classifications.")
      )
    ),
    context = "add_atc2_classification"
  )
}

#' Get unique RxCUIs from data frame
#' @noRd
.get_unique_rxcuis <- function(dataf, rxcui_col) {
  # Get all RxCUI values
  rxcuis <- dataf[[rxcui_col]]

  # Filter to non-NA values and convert to character
  valid_rxcuis <- rxcuis[!is.na(rxcuis)]
  unique_rxcuis <- unique(as.character(valid_rxcuis))

  cli::cli_alert_info("Found {length(unique_rxcuis)} unique RxCUIs out of {length(valid_rxcuis)} total valid entries")

  return(list(
    unique_rxcuis = unique_rxcuis,
    count = length(unique_rxcuis),
    total_valid = length(valid_rxcuis)
  ))
}

#' Apply ATC2 results in nested format (multiple classes joined with semicolons)
#' @noRd
.apply_atc2_nested <- function(dataf, rxcui_col, new_col_name, rxcui_to_atc2) {

  dataf[[new_col_name]] <- sapply(dataf[[rxcui_col]], function(rxcui) {
    if (is.na(rxcui)) return(NA_character_)

    atc2_class <- rxcui_to_atc2[[as.character(rxcui)]]

    if (is.null(atc2_class) || all(is.na(atc2_class))) {
      NA_character_
    } else if (length(atc2_class) > 1) {
      paste(atc2_class, collapse = "; ")
    } else {
      as.character(atc2_class)
    }
  })

  return(dataf)
}

#' Apply ATC2 results in unnested format (separate rows for each class)
#' @noRd
.apply_atc2_unnested <- function(dataf, rxcui_col, new_col_name, rxcui_to_atc2) {

  # Create mapping data frame (unnested - separate rows)
  atc2_mapping <- do.call(rbind, lapply(names(rxcui_to_atc2), function(rxcui) {
    atc2_classes <- rxcui_to_atc2[[rxcui]]

    if (is.null(atc2_classes) || all(is.na(atc2_classes))) {
      data.frame(rxcui = rxcui, atc2_class = NA_character_, stringsAsFactors = FALSE)
    } else {
      data.frame(rxcui = rxcui, atc2_class = atc2_classes, stringsAsFactors = FALSE)
    }
  }))

  # Rename column if needed
  if (new_col_name != "atc2_class") {
    names(atc2_mapping)[names(atc2_mapping) == "atc2_class"] <- new_col_name
  }

  # Join with original data
  dataf <- dplyr::mutate(dataf, .row_id = dplyr::row_number())
  result_df <- dataf %>%
    dplyr::left_join(atc2_mapping, by = setNames("rxcui", rxcui_col), relationship = "many-to-many")

  return(result_df)
}

#' Report ATC2 processing results
#' @noRd
.report_atc2_results <- function(result_df, new_col_name, start_time = NULL) {

  total_rows <- nrow(result_df)
  found_count <- sum(!is.na(result_df[[new_col_name]]))
  coverage_pct <- round(found_count / total_rows * 100, 1)

  cli::cli_h2(cli::col_cyan("ATC2 RESULTS"))
  cli_alert_info("Method: ATC2 lookup")
  cli_alert_info("Coverage: {found_count}/{total_rows} ({coverage_pct}%)")

  # Show most common ATC2 classes
  if (found_count > 0) {
    atc2_values <- result_df[[new_col_name]]
    atc2_values <- atc2_values[!is.na(atc2_values)]

    # Handle both nested (with semicolons) and unnested formats
    if (any(grepl(";", atc2_values))) {
      # Split semicolon-separated values
      all_classes <- unlist(strsplit(atc2_values, ";\\s*"))
    } else {
      all_classes <- atc2_values
    }

    class_counts <- table(all_classes)
    top_classes <- head(sort(class_counts, decreasing = TRUE), 5)

    cli::cli_alert_info("Top ATC2 classes found:")
    cli::cli_ul()
    for (i in seq_along(top_classes)) {
      cli::cli_li("{cli::col_blue(names(top_classes)[i])}: {top_classes[i]} medications")
    }
    cli::cli_end()
  }

  if (!is.null(start_time)) {
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    cli_alert_info("Time: {round(elapsed, 1)} minutes")
  }
  cli_rule()
}
