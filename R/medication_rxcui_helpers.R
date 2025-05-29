#' Process medications in batches with caching and progress tracking
#'
#' Processes a list of medication names through the RxNorm API in manageable
#' batches, with periodic cache saves to prevent data loss and rate limiting
#' to respect API constraints.
#'
#' @param to_process Character vector of medication names to process
#' @param method Character. Either "exact" or "approximate" matching
#' @param batch_size Integer. Number of medications per batch
#' @param save_freq Integer. Save cache every N processed items
#' @param retry_count Integer. Number of retry attempts per medication
#' @param batch_delay Numeric. Seconds to wait between batches
#' @param cache Named list. Existing cache to update
#' @param cache_path Character. File path for cache storage
#'
#' @return Updated cache as a named list
#' @keywords internal
process_medications_batch <- function(to_process, method, batch_size,
                                      save_freq, retry_count,
                                      batch_delay, cache, cache_path) {
  num_batches <- ceiling(length(to_process) / batch_size)

  for (i in 1:num_batches) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, length(to_process))
    batch_meds <- to_process[start_idx:end_idx]

    message("Processing batch ", i, "/", num_batches, " (", length(batch_meds), " medications)")
    pb <- txtProgressBar(min = 0, max = length(batch_meds), style = 3)

    for (j in seq_along(batch_meds)) {
      med_name <- batch_meds[j]
      setTxtProgressBar(pb, j)

      rxcui <- get_single_rxcui(med_name, method, retry_count)
      cache[[med_name]] <- rxcui

      # Save every X items
      if (j %% save_freq == 0) {
        saveRDS(list(cache = cache, timestamp = Sys.time()), cache_path)
      }

      Sys.sleep(0.1)  # Rate limiting
    }

    close(pb)

    # Save after each batch
    saveRDS(list(cache = cache, timestamp = Sys.time()), cache_path)

    if (i < num_batches) {
      message("Pausing between batches...")
      Sys.sleep(batch_delay)
    }
  }

  return(cache)
}

#' Get single RxCUI from RxNorm API with retry logic
#'
#' Makes a single API call to RxNorm to retrieve the RxCUI for one medication,
#' with automatic retry logic for failed requests and proper error handling.
#'
#' @param med_name Character. Single medication name to look up
#' @param method Character. Either "exact" or "approximate" matching
#' @param retry_count Integer. Number of retry attempts on failure
#'
#' @return Character. RxCUI if found, NA_character_ if not found or error
#' @keywords internal
#' @export
get_single_rxcui <- function(med_name, method, retry_count) {
  for (attempt in 1:retry_count) {
    rxcui <- tryCatch({
      if (method == "exact") {
        url <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?name=",
                      utils::URLencode(med_name), "&search=2")
      } else {
        url <- paste0("https://rxnav.nlm.nih.gov/REST/approximateTerm.json?term=",
                      utils::URLencode(med_name))
      }

      response <- httr::GET(url, httr::timeout(10))

      if (httr::status_code(response) == 200) {
        json_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

        if (method == "exact") {
          null_coalesce(json_data$idGroup$rxnorm[1], NA_character_)
        } else {
          null_coalesce(json_data$approximateGroup$candidate$rxcui[1], NA_character_)
        }
      } else {
        NA_character_
      }
    }, error = function(e) {
      if (attempt == retry_count) {
        message("\nError processing '", med_name, "': ", e$message)
      }
      NA_character_
    })

    if (!is.na(rxcui)) break
    if (attempt < retry_count) Sys.sleep(1)
  }

  return(rxcui)
}

#' Report final processing results with timing and coverage statistics
#'
#' Generates a formatted summary of RxCUI processing results including
#' coverage percentage, method used, and total processing time.
#'
#' @param result_df Data frame with RxCUI results
#' @param original_df Original input data frame
#' @param rxcui_column Character. Name of RxCUI column
#' @param med_column Character. Name of medication column
#' @param start_time POSIXct. Processing start time
#' @param method Character. Method used ("exact" or "approximate")
#'
#' @return NULL (prints results to console)
#' @keywords internal
report_results <- function(result_df, original_df, rxcui_column, med_column, start_time, method) {
  found_count <- sum(!is.na(result_df[[rxcui_column]]))
  valid_count <- sum(!is.na(original_df[[med_column]]) & original_df[[med_column]] != "")

  message("\n=== RESULTS ===")
  message("Method: ", method)
  message("Coverage: ", found_count, "/", valid_count, " (", round(found_count/valid_count*100, 1), "%)")

  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  message("Time: ", round(elapsed, 1), " minutes")
  message("===============")
}

#' Null-coalescing operator
#'
#' Returns the right-hand side value if the left-hand side is NULL or NA,
#' otherwise returns the left-hand side value. Similar to `||` in other languages.
#'
#' @param x Any value to test
#' @param y Default value to return if x is NULL or NA
#'
#' @return x if not NULL/NA, otherwise y
#' @keywords internal
null_coalesce <- function(x, y) if (is.null(x) || is.na(x)) y else x
