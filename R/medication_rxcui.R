#' Get RxCUIs for medications using JSON extraction
#'
#' @param dataf Data frame containing medication names
#' @param med_column Character. Column containing medication names
#' @param rxcui_column Character. Name for the output RxCUI column (default: "rxcui")
#' @param batch_size Numeric. Medications per batch (default: 200)
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param force_reprocess Logical. Force reprocessing (default: FALSE)
#'
#' @return Updated data frame with RxCUI column
#' @export
add_medication_rxcuis <- function(dataf,
                                       med_column,
                                       rxcui_column = "rxcui",
                                       batch_size = 200,
                                       cache_dir = "cache",
                                       force_reprocess = FALSE) {

  # Start timing
  start_time <- Sys.time()

  # Validate inputs
  if (!is.data.frame(dataf)) stop("'dataf' must be a data frame")
  if (!med_column %in% names(dataf)) stop(paste("Column", med_column, "not found in data"))

  # Initialize cache
  cache_name <- "rxcui_json_cache"
  cache_path <- file.path(cache_dir, paste0(cache_name, ".rds"))

  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Load or initialize cache
  if (file.exists(cache_path) && !force_reprocess) {
    message("Loading existing cache...")
    cache <- readRDS(cache_path)
  } else {
    message("Initializing new cache...")
    cache <- list()
  }

  # Create output column
  result_df <- dataf
  if (!(rxcui_column %in% names(result_df))) {
    result_df[[rxcui_column]] <- NA_character_
  }

  # Get unique, non-NA medication names
  message("Extracting unique medication names...")
  meds <- dataf[[med_column]]
  unique_meds <- unique(meds[!is.na(meds) & meds != ""])

  message("Found ", length(unique_meds), " unique medications out of ", sum(!is.na(meds)), " total entries")

  if (length(unique_meds) == 0) {
    message("No valid medication names found")
    return(result_df)
  }

  # Determine which medications need to be processed
  to_process <- character(0)
  for (med in unique_meds) {
    if (!(med %in% names(cache))) {
      to_process <- c(to_process, med)
    }
  }

  message(length(to_process), " medications need API lookups, ",
          length(unique_meds) - length(to_process), " found in cache")

  # Process medications in batches
  if (length(to_process) > 0) {
    message("Processing ", length(to_process), " unique medications...")
    num_batches <- ceiling(length(to_process) / batch_size)

    for (i in 1:num_batches) {
      # Get batch of medications
      start_idx <- (i-1) * batch_size + 1
      end_idx <- min(i * batch_size, length(to_process))
      batch_meds <- to_process[start_idx:end_idx]

      message("Processing batch ", i, "/", num_batches, " (", length(batch_meds), " medications)")
      pb <- txtProgressBar(min = 0, max = length(batch_meds), style = 3)

      for (j in seq_along(batch_meds)) {
        med_name <- batch_meds[j]
        setTxtProgressBar(pb, j)

        # Make API call and extract RxCUI
        rxcui <- NA_character_

        tryCatch({
          # Build URL
          url <- paste0("https://rxnav.nlm.nih.gov/REST/approximateTerm?term=",
                        utils::URLencode(med_name))

          # Make request
          response <- httr::GET(url)

          if (httr::status_code(response) == 200) {
            # Get response as text
            response_text <- httr::content(response, "text", encoding = "UTF-8")

            # Parse JSON directly using jsonlite
            json_data <- jsonlite::fromJSON(response_text)

            # Extract RxCUI using direct JSON object access
            if (!is.null(json_data$approximateGroup) &&
                !is.null(json_data$approximateGroup$candidate) &&
                length(json_data$approximateGroup$candidate) > 0 &&
                !is.null(json_data$approximateGroup$candidate$rxcui)) {

              # Get the first RxCUI (top match)
              rxcui <- json_data$approximateGroup$candidate$rxcui[1]
            }
          }
        }, error = function(e) {
          message("\nError processing '", med_name, "': ", e$message)
        })

        # Add to cache
        cache[[med_name]] <- rxcui

        # Small delay to avoid rate limiting
        Sys.sleep(0.1)
      }

      close(pb)

      # Save cache after each batch
      message("Saving cache...")
      saveRDS(cache, cache_path)

      # Pause between batches
      if (i < num_batches) {
        message("Pausing between batches...")
        Sys.sleep(2)
      }
    }
  } else {
    message("All medications found in cache")
  }

  # Apply cached RxCUIs to the df
  message("Applying RxCUIs to dataframe...")

  # Create a lookup vector
  rxcui_lookup <- sapply(unique_meds, function(med) cache[[med]])
  names(rxcui_lookup) <- unique_meds

  # Apply to dataframe in one operation
  result_df[[rxcui_column]] <- rxcui_lookup[result_df[[med_column]]]

  # Report results
  found_count <- sum(!is.na(result_df[[rxcui_column]]))
  valid_count <- sum(!is.na(dataf[[med_column]]) & dataf[[med_column]] != "")

  message("Found RxCUIs for ", found_count, "/", valid_count, " medications (",
          round(found_count/valid_count*100, 1), "% coverage)")

  # Report timing
  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")
  message("Processing completed in ", round(elapsed, 1), " minutes")

  return(result_df)
}
