#' Get RxNorm RxCUIs for medication names in any dataset
#'
#' @description
#' A generalized function to retrieve RxCUI codes for medication names
#' in any dataset, with caching and handling of existing RxCUI columns.
#'
#' @param data A data frame containing medication names
#' @param med_column Character. Column containing medication names
#' @param rxcui_column Character. Name for the output RxCUI column (default: "rxcui")
#' @param append Logical. If TRUE and rxcui_column exists, adds new column with suffix (default: TRUE)
#' @param batch_size Numeric. Medications per batch (default: 100)
#' @param cache_name Character. Cache name (default: "rxcui_cache")
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param save_freq Numeric. Save frequency (default: 100)
#' @param force_reprocess Logical. Force reprocessing (default: FALSE)
#' @param sleep_time Numeric. Sleep time between API calls in seconds (default: 0.1)
#'
#' @return Updated data frame with RxCUI column
#' @export
get_medication_rxcuis <- function(dataf, med_column, rxcui_column = "rxcui",
                                  append = TRUE, batch_size = 500,
                                  cache_name = "rxcui_cache", cache_dir = "cache",
                                  save_freq = 100, force_reprocess = FALSE,
                                  sleep_time = 0.1) {

  # Validate inputs
  if (!is.data.frame(dataf)) stop("'data' must be a data frame")
  if (!med_column %in% names(dataf)) stop(paste("Column", med_column, "not found in data"))

  # Generate a unique tracking ID combining data dimensions and column name
  # This helps avoid conflicts when processing different datasets with same column names
  tracking_id <- paste0(med_column, "_", nrow(dataf), "_", ncol(dataf))

  # Check if already processed with this exact tracking ID
  if (!force_reprocess && is_processed("get_medication_rxcuis", tracking_id)) {
    message("This specific dataset's '", med_column, "' column already processed. Use force_reprocess=TRUE to override.")
    return(dataf)
  }

  # Handle case where rxcui_column already exists
  original_rxcui_column <- rxcui_column
  if (rxcui_column %in% names(dataf) && append) {
    # Create a new column name with suffix
    i <- 1
    while(paste0(rxcui_column, "_", i) %in% names(dataf)) {
      i <- i + 1
    }
    rxcui_column <- paste0(rxcui_column, "_", i)
    message("Column '", original_rxcui_column, "' already exists. Using '", rxcui_column, "' instead.")
  } else if (rxcui_column %in% names(dataf) && !append) {
    message("Column '", rxcui_column, "' already exists and will be updated.")
  }

  # Initialize cache
  cache <- adRutils::initialize_cache(cache_name, cache_dir)

  # Create or prepare output column
  if (!(rxcui_column %in% names(dataf))) {
    dataf[[rxcui_column]] <- NA_character_
  }

  # Get unique medications that need processing
  meds <- dataf[[med_column]]

  # Skip NA medications
  valid_med_idx <- which(!is.na(meds))

  # Also skip empty strings and blanks
  valid_med_idx <- valid_med_idx[nchar(trimws(meds[valid_med_idx])) > 0]

  if (length(valid_med_idx) == 0) {
    message("No valid medication names found in column '", med_column, "'")
    adRutils::register_processed("get_medication_rxcuis", tracking_id)
    return(dataf)
  }

  # Identify which medications need RxCUI lookup
  to_process_idx <- valid_med_idx[is.na(dataf[[rxcui_column]][valid_med_idx])]

  if (length(to_process_idx) == 0) {
    message("All valid medications already have RxCUIs")
    adRutils::register_processed("get_medication_rxcuis", tracking_id)
    return(dataf)
  }

  # Get unique medication names for processing
  unique_meds <- unique(meds[to_process_idx])
  total_unique <- length(unique_meds)

  # Process in batches
  message("Processing ", total_unique, " unique medications from '", med_column, "'")
  num_batches <- ceiling(total_unique / batch_size)
  counter <- 0

  # Create progress bar
  pb <- txtProgressBar(min = 0, max = total_unique, style = 3)

  # Store any errors for reporting
  errors <- character(0)

  for (i in 1:num_batches) {
    # Get batch of medications
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, total_unique)
    batch_meds <- unique_meds[start_idx:end_idx]

    for (med_name in batch_meds) {
      # Update progress
      counter <- counter + 1
      setTxtProgressBar(pb, counter)

      # Check cache first
      rxcui <- adRutils::get_from_cache(cache, med_name)

      if (is.null(rxcui)) {
        # API lookup
        tryCatch({
          result <- rxnorm::find_approx_rxcui(med_name)
          rxcui <- if (length(result) > 0 && !is.na(result[1])) as.character(result[1]) else NA_character_

          # Add to cache
          cache <- adRutils::add_to_cache(cache, med_name, rxcui)

          # Save cache periodically
          adRutils::save_cache(cache, cache_name, cache_dir, periodic = TRUE,
                               save_frequency = save_freq, counter = counter)

          # Avoid rate limiting
          Sys.sleep(sleep_time)
        },
        error = function(e) {
          # Record errors for later reporting
          errors <- c(errors, paste("Error with '", med_name, "': ", e$message))
          rxcui <- NA_character_
        })
      }

      # Update dataframe for all matching medications
      dataf[[rxcui_column]][meds == med_name] <- rxcui
    }
  }

  close(pb)

  # Final cache save
  adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE)

  # Register as processed
  adRutils::register_processed("get_medication_rxcuis", tracking_id)

  # Report results
  found_count <- sum(!is.na(dataf[[rxcui_column]][valid_med_idx]))
  message("\nFound RxCUIs for ", found_count, "/", length(valid_med_idx), " medications (",
          round(found_count/length(valid_med_idx)*100, 1), "% coverage)")

  # Report any errors
  if (length(errors) > 0) {
    message("\nEncountered ", length(errors), " errors during processing:")
    for (i in 1:min(5, length(errors))) {
      message(" - ", errors[i])
    }
    if (length(errors) > 5) {
      message(" - ... and ", length(errors) - 5, " more errors")
    }
  }
  return(dataf)
}
