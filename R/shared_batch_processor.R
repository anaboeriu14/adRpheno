

#' Unified batch processor for medication API calls
#'
#' Process items in batches with caching and progress tracking using adRutils cache
#'
#' @param items Character vector of items to process
#' @param api_function Function that processes a single item
#' @param batch_size Number of items per batch (default: 200)
#' @param save_freq Save cache every N items (default: 50)
#' @param cache_name Cache file name (default: "api_cache")
#' @param cache_dir Cache directory (default: "cache")
#' @param max_age_days Cache expiration in days (default: 30)
#' @param retry_count Retry attempts for failed calls (default: 3)
#' @param batch_delay Seconds between batches (default: 2)
#' @param process_type Description for progress messages (default: "items")
#' @param ... Additional arguments passed to api_function
#' @return Named list of results or cache object
#' @keywords internal

.process_batch <- function(items, api_function, batch_size = 200, save_freq = 50,
                           cache_name = "api_cache", cache_dir = "cache",
                           max_age_days = 30, retry_count = 3, batch_delay = 2,
                           process_type = "items", ...) {

  # Initialize cache using adRutils
  cache <- adRutils::initialize_cache(cache_name, cache_dir)

  # Clean expired entries if cache is old
  if (adRutils::is_cache_expired(cache, max_age_days)) {
    cache <- adRutils::clean_cache(cache, max_age_days)
  }

  # Filter items that need processing
  items_to_process <- character()
  cache_results <- list()

  for (item in items) {
    cached_value <- adRutils::get_from_cache(cache, item, default = NULL, max_age_days = max_age_days)
    if (is.null(cached_value)) {
      items_to_process <- c(items_to_process, item)
    } else {
      cache_results[[item]] <- cached_value
    }
  }

  if (length(items_to_process) == 0) {
    message("All ", process_type, " found in cache")
    return(cache_results[items])
  }

  # Show cache statistics
  stats <- adRutils::get_cache_stats(cache)
  message("Cache contains ", stats$entry_count, " existing entries")

  # Create batches
  batches <- split(items_to_process, ceiling(seq_along(items_to_process) / batch_size))
  message("Processing ", length(items_to_process), " new ", process_type, " in ", length(batches), " batches")

  # Initialize CLI progress bar - track by items, not batches
  cli::cli_progress_bar(
    format = "Processing {process_type} [{cli::pb_bar}] {cli::pb_percent} | Batch {.val {current_batch}}/{.val {length(batches)}}",
    total = length(items_to_process),
    clear = FALSE
  )

  current_batch <- 1
  processed_count <- 0
  cli::cli_progress_update(set = 0)

  # Process each batch
  for (i in seq_along(batches)) {
    batch <- batches[[i]]
    current_batch <- i

    # Update status to show current batch being processed
    cli::cli_progress_update(
      set = processed_count,
      status = sprintf("Processing batch %d/%d (%d items)...", i, length(batches), length(batch))
    )

    # Process batch with retry logic
    batch_results <- .process_one_batch(batch, api_function, retry_count, ...)

    # Add results to cache using adRutils
    for (item in names(batch_results)) {
      cache <- adRutils::add_to_cache(cache, item, batch_results[[item]])
      cache_results[[item]] <- batch_results[[item]]
    }

    # Update processed count and progress bar
    processed_count <- processed_count + length(batch)
    cli::cli_progress_update(set = processed_count)

    # Save cache periodically using adRutils
    if (i %% save_freq == 0 || i == length(batches)) {
      adRutils::save_cache(
        cache = cache,
        cache_name = cache_name,
        cache_dir = cache_dir,
        force = TRUE,
        periodic = FALSE
      )
    }

    # Delay between batches (except last one)
    if (i < length(batches) && batch_delay > 0) {
      Sys.sleep(batch_delay)
    }
  }

  # Complete progress bar
  cli::cli_progress_done()

  message("Processing complete: ", length(items_to_process), " new ", process_type, " processed")

  # Return results for all requested items (both cached and newly processed)
  return(cache_results[items])
}

#' Process a single batch with retry logic
#' @keywords internal
.process_one_batch <- function(batch, api_function, retry_count, ...) {

  batch_results <- list()

  for (item in batch) {
    result <- NULL
    attempts <- 0

    while (is.null(result) && attempts < retry_count) {
      attempts <- attempts + 1

      tryCatch({
        result <- api_function(item, ...)
        if (!is.null(result)) {
          batch_results[[item]] <- result
        }
      }, error = function(e) {
        if (attempts == retry_count) {
          warning("Failed to process '", item, "' after ", retry_count, " attempts: ", e$message)
          batch_results[[item]] <- NA
        } else {
          Sys.sleep(1)  # Brief pause before retry
        }
      })
    }

    # Set NA if all attempts failed
    if (is.null(result)) {
      batch_results[[item]] <- NA
    }
  }

  return(batch_results)
}
