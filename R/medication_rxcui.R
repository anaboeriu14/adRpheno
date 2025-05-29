#' Get RxCUIs using exact or approximate matching with intelligent caching
#'
#' @param dataf Data frame containing medication names
#' @param med_column Character. Column containing medication names
#' @param rxcui_column Character. Name for the RxCUI output column (default: "rxcui")
#' @param method Character. "exact" or "approximate" (default: "exact")
#' @param batch_size Integer. Number of medications per batch (default: 200)
#' @param save_freq Integer. Save cache every N processed items (default: 50)
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param force_reprocess Logical. Force reprocessing of cached items (default: FALSE)
#' @param max_age_days Integer. Cache expiration in days (default: 30)
#' @param retry_count Integer. Number of retry attempts (default: 3)
#' @param batch_delay Numeric. Seconds between batches (default: 2)
#'
#' @return Data frame with RxCUI column added
#' @export
add_rxcuis <- function(dataf, med_column, rxcui_column = "rxcui",
                       method = "exact",
                       batch_size = 200,
                       save_freq = 50,
                       cache_dir = "cache",
                       force_reprocess = FALSE,
                       max_age_days = 30,
                       retry_count = 3,
                       batch_delay = 2) {

  start_time <- Sys.time()

  # Validate inputs
  if (!is.data.frame(dataf)) stop("'dataf' must be a data frame")
  if (!med_column %in% names(dataf)) stop(paste("Column", med_column, "not found in data"))
  if (!method %in% c("exact", "approximate")) stop("method must be 'exact' or 'approximate'")

  # Initialize cache with timestamp tracking
  cache_name <- paste0("rxcui_", method, "_cache")
  cache_path <- file.path(cache_dir, paste0(cache_name, ".rds"))

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Load cache and clean old entries
  if (file.exists(cache_path) && !force_reprocess) {
    message("Loading existing cache...")
    cache_data <- readRDS(cache_path)
    cache <- cache_data$cache
    cache_timestamp <- cache_data$timestamp

    # Clean expired entries
    if (!is.null(cache_timestamp)) {
      days_old <- as.numeric(difftime(Sys.time(), cache_timestamp, units = "days"))
      if (days_old > max_age_days) {
        message("Cache is ", round(days_old, 1), " days old. Cleaning expired entries...")
        cache <- list()
      }
    }
  } else {
    message("Initializing new cache...")
    cache <- list()
  }

  # Prepare result dataframe
  result_df <- dataf
  if (!(rxcui_column %in% names(result_df))) {
    result_df[[rxcui_column]] <- NA_character_
  }

  # Get unique medications
  meds <- dataf[[med_column]]
  unique_meds <- unique(meds[!is.na(meds) & meds != ""])

  message("Found ", length(unique_meds), " unique medications out of ", sum(!is.na(meds)), " total entries")

  if (length(unique_meds) == 0) {
    message("No valid medication names found")
    return(result_df)
  }

  # Determine which medications need processing
  to_process <- unique_meds[!unique_meds %in% names(cache)]

  message(length(to_process), " medications need API lookups, ",
          length(unique_meds) - length(to_process), " found in cache")

  # Process in batches
  if (length(to_process) > 0) {
    cache <- process_medications_batch(to_process, method, batch_size, save_freq,
                                       retry_count, batch_delay, cache, cache_path)
  }

  # Apply results to dataframe
  message("Applying RxCUIs to dataframe...")
  rxcui_lookup <- character(length(unique_meds))
  names(rxcui_lookup) <- unique_meds
  for (med in unique_meds) {
    rxcui_lookup[med] <- null_coalesce(cache[[med]], NA_character_)
  }
  result_df[[rxcui_column]] <- rxcui_lookup[result_df[[med_column]]]

  # Save final cache with timestamp
  saveRDS(list(cache = cache, timestamp = Sys.time()), cache_path)

  # Report results
  report_results(result_df, dataf, rxcui_column, med_column, start_time, method)

  return(result_df)
}
