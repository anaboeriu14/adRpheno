#' Core ATC2 processing logic
#'
#' Internal function that handles the common batch processing and API logic
#' for both nested and unnested ATC2 classification functions.
#'
#' @param unique_rxcuis Character vector of unique RxCUI values to process
#' @param batch_size Number of RxCUIs to process in each batch (default: 200)
#' @param save_freq Frequency of how often to save cache within each batch (default: 50)
#' @param cache_name File name for the cache (default: "atc2_cache")
#' @param cache_dir Directory name for the cache (default: "cache")
#' @param max_age_days Maximum age of cached entries in days (default: 30)
#' @param retry_count Number of times to retry failed API calls (default: 3)
#' @param retry_delay Seconds to wait between retries (default: 1)
#' @param batch_delay Seconds to wait between batches (default: 2)
#'
#' @return Named list mapping RxCUI to ATC2 classifications
#' @keywords internal
process_atc2_batch <- function(unique_rxcuis,
                               batch_size = 200, save_freq = 50,
                               cache_name = "atc2_cache", cache_dir = "cache",
                               max_age_days = 30, retry_count = 3,
                               retry_delay = 1, batch_delay = 2) {

  # Initialize cache and clean old entries
  cache <- adRutils::initialize_cache(cache_name, cache_dir, ".rds")
  cache <- adRutils::clean_cache(cache, max_age_days)

  # Split RxCUIs into batches
  total_rxcuis <- length(unique_rxcuis)
  num_batches <- ceiling(total_rxcuis / batch_size)

  message("Getting ATC2 classifications for ", total_rxcuis, " unique RxCUIs in ", num_batches, " batches")

  rxcui_to_atc2 <- list()

  # Process in batches
  for (batch_num in seq_len(num_batches)) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, total_rxcuis)
    batch_rxcuis <- unique_rxcuis[start_idx:end_idx]

    message(sprintf("Processing batch %d/%d (%d RxCUIs)",
                    batch_num, num_batches, length(batch_rxcuis)))

    batch_new_entries <- 0

    for (i in seq_along(batch_rxcuis)) {
      rxcui <- batch_rxcuis[i]

      # Check cache directly
      atc2_classes <- adRutils::get_from_cache(cache, rxcui, default = NULL, max_age_days = max_age_days)

      if (is.null(atc2_classes)) {
        # API call with retries
        for (attempt in 1:retry_count) {
          atc2_classes <- tryCatch({
            Sys.sleep(0.1)
            rxnorm::get_atc(rxcui, "second")
          }, error = function(e) {
            if (attempt == retry_count) message("\nFinal error for RxCUI '", rxcui, "': ", e$message)
            NA_character_
          })

          if (!all(is.na(atc2_classes)) && length(atc2_classes) > 0) break
          if (attempt < retry_count) Sys.sleep(retry_delay)
        }

        # Cache the result
        cache <- adRutils::add_to_cache(cache, rxcui, atc2_classes)
        batch_new_entries <- batch_new_entries + 1

        if (batch_new_entries %% save_freq == 0) {
          adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE, compress = TRUE)
        }
      }

      rxcui_to_atc2[[rxcui]] <- atc2_classes
    }

    # Save cache at end of batch
    if (batch_new_entries > 0) {
      adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE, compress = TRUE)
    }

    # Wait between batches
    if (batch_num < num_batches) {
      Sys.sleep(batch_delay)
    }
  }

  # Output summary
  cache_stats <- adRutils::get_cache_stats(cache)
  message(sprintf("ATC2 processing complete! Cache statistics: %d entries, %.1f days old",
                  cache_stats$entry_count,
                  if(is.na(cache_stats$age_days)) 0 else cache_stats$age_days))

  return(rxcui_to_atc2)
}

#' Add ATC2 classifications to medications (nested version)
#' @keywords internal
add_medication_atc2_nested <- function(dataf, rxcui_col = "rxcui",
                                       new_col_name = "atc2_class",
                                       ...) {
  # Validation
  if (!rxcui_col %in% names(dataf)) {
    stop(sprintf("Column '%s' not found in the data frame.", rxcui_col))
  }

  unique_rxcuis <- unique(as.character(dataf[[rxcui_col]][!is.na(dataf[[rxcui_col]])]))

  if (length(unique_rxcuis) == 0) {
    warning("No valid RxCUIs found in the data frame")
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # Process RxCUIs
  rxcui_to_atc2 <- process_atc2_batch(unique_rxcuis, ...)

  # Apply to data frame (nested - join with semicolons)
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

#' Add ATC2 classifications to medications (unnested version)
#' @keywords internal
add_medication_atc2_unnested <- function(dataf, rxcui_col = "rxcui", new_col_name = "atc2_class", ...) {

  if (!rxcui_col %in% names(dataf)) {
    stop(sprintf("Column '%s' not found in the data frame.", rxcui_col))
  }

  unique_rxcuis <- unique(as.character(dataf[[rxcui_col]][!is.na(dataf[[rxcui_col]])]))

  if (length(unique_rxcuis) == 0) {
    warning("No valid RxCUIs found in the data frame")
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # Process RxCUIs
  rxcui_to_atc2 <- process_atc2_batch(unique_rxcuis, ...)

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

  message("Created unnested data frame with ATC2 classifications")
  return(result_df)
}
