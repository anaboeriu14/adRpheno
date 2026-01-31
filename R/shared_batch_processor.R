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

  # Initialize progress bar for ALL items (including cached)
  total_items <- length(items)
  pb_id <- cli::cli_progress_bar(
    format = paste0("Processing ", process_type, " [{cli::pb_bar}] {cli::pb_current}/{cli::pb_total}"),
    total = total_items,
    clear = FALSE
  )

  # Filter items that need processing, updating progress for cached items
  items_to_process <- vector("character", total_items)
  cache_results <- vector("list", total_items)
  names(cache_results) <- items
  process_idx <- 0
  cached_count <- 0

  for (i in seq_along(items)) {
    item <- items[i]
    cached_value <- adRutils::get_from_cache(cache, item, default = NULL, max_age_days = max_age_days)

    if (is.null(cached_value)) {
      process_idx <- process_idx + 1
      items_to_process[process_idx] <- item
    } else {
      cache_results[[item]] <- cached_value
      cached_count <- cached_count + 1
      cli::cli_progress_update(id = pb_id, inc = 1)
    }
  }

  # Trim to actual size needed
  items_to_process <- items_to_process[seq_len(process_idx)]

  if (length(items_to_process) == 0) {
    cli::cli_progress_done(id = pb_id)
    cli::cli_alert_success("All {total_items} {process_type} retrieved from cache")
    return(vapply(cache_results[items], function(x) {
      if (is.list(x) && length(x) == 1) {
        as.character(x[[1]])
      } else {
        as.character(x)
      }
    }, FUN.VALUE = character(1), USE.NAMES = TRUE))
  }

  # Show cache statistics
  if (cached_count > 0) {
    cli::cli_alert_info("{cached_count}/{total_items} {process_type} found in cache")
  }

  # Create batches
  batches <- split(items_to_process, ceiling(seq_along(items_to_process) / batch_size))
  cli::cli_alert_info("Processing {length(items_to_process)} new {process_type} in {length(batches)} batch{?es}")

  # Calculate save frequency in terms of batches
  save_every_n_batches <- max(1, ceiling(save_freq / batch_size))

  # Process each batch
  for (i in seq_along(batches)) {
    batch <- batches[[i]]

    # Process batch
    batch_results <- .process_one_batch(batch, api_function, retry_count, pb_id, ...)

    # Add results to cache
    for (item in names(batch_results)) {
      cache <- adRutils::add_to_cache(cache, item, batch_results[[item]])
      cache_results[[item]] <- batch_results[[item]]
    }

    # Save cache periodically (every X batches)
    if (i %% save_every_n_batches == 0 || i == length(batches)) {
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
  cli::cli_progress_done(id = pb_id)

  cli::cli_alert_success("Processed {length(items_to_process)} new {process_type} ({cached_count} from cache)")

  final_results <- vapply(cache_results[items], function(x) {
    if (is.list(x) && length(x) == 1) {
      as.character(x[[1]])
    } else {
      as.character(x)
    }
  }, FUN.VALUE = character(1), USE.NAMES = TRUE)

  return(final_results)
}

#' Process a single batch with retry logic
#' @keywords internal
.process_one_batch <- function(batch, api_function, retry_count, pb_id, ...) {
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
          cli_alert_warning("Failed to process {.strong {item}} after {retry_count} attempts: {e$message}")
          batch_results[[item]] <- NA
        } else {
          Sys.sleep(1)
        }
      })
    }

    if (is.null(result)) {
      batch_results[[item]] <- NA
    }

    cli::cli_progress_update(id = pb_id, inc = 1)
  }

  return(batch_results)
}
