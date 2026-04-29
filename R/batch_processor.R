#' Process items in batches with caching, retry, and progress reporting
#'
#' Internal orchestration helper that wraps an `api_function` (one call per
#' item) with on-disk caching ([initialize_cache()] et al.), batched
#' processing, retry-on-failure, and a `cli` progress bar.
#'
#' @param items Character vector of items to process.
#' @param api_function Function that processes a single item. Must accept the
#'   item as its first argument and return a value (or `NULL`/`NA` on failure).
#' @param batch_size Items per batch (default: 200).
#' @param save_freq Save cache every ~N items (default: 50).
#' @param cache_name Cache file name stem (default: `"api_cache"`).
#' @param cache_dir Cache directory (default: `"cache"`).
#' @param max_age_days Cache expiration in days (default: 30).
#' @param retry_count Retry attempts for failed calls (default: 3).
#' @param batch_delay Seconds between batches (default: 2).
#' @param process_type Description for progress messages (default: `"items"`).
#' @param ... Additional arguments passed to `api_function`.
#'
#' @return Named character vector of results, one entry per input item.
#' @keywords internal
#' @noRd
.process_batch <- function(items, api_function, batch_size = 200, save_freq = 50,
                           cache_name = "api_cache", cache_dir = "cache",
                           max_age_days = 30, retry_count = 3, batch_delay = 2,
                           process_type = "items", ...) {

  # Initialize and (if needed) clean the cache
  cache <- initialize_cache(cache_name, cache_dir)
  if (is_cache_expired(cache, max_age_days)) {
    cache <- clean_cache(cache, max_age_days)
  }

  total_items <- length(items)

  # Single progress bar covers cached + new items
  pb_id <- cli::cli_progress_bar(
    format = paste0("Processing ", process_type,
                    " [{cli::pb_bar}] {cli::pb_current}/{cli::pb_total}"),
    total  = total_items,
    clear  = FALSE
  )

  # Partition items into "already cached" vs "needs processing", advancing
  # the progress bar for each cached hit.
  partition <- .partition_items_by_cache(items, cache, max_age_days, pb_id)
  cache_results   <- partition$cache_results
  items_to_process <- partition$items_to_process
  cached_count     <- partition$cached_count

  # Fast path: everything was cached
  if (length(items_to_process) == 0L) {
    cli::cli_progress_done(id = pb_id)
    cli::cli_alert_success("All {total_items} {process_type} retrieved from cache")
    return(.flatten_results(cache_results, items))
  }

  if (cached_count > 0L) {
    cli::cli_alert_info("{cached_count}/{total_items} {process_type} found in cache")
  }

  # Split remaining items into batches and process
  batches <- split(items_to_process,
                   ceiling(seq_along(items_to_process) / batch_size))
  cli::cli_alert_info(
    "Processing {length(items_to_process)} new {process_type} in {length(batches)} batch{?es}"
  )

  save_every_n_batches <- max(1L, ceiling(save_freq / batch_size))

  for (i in seq_along(batches)) {
    batch_results <- .process_one_batch(batches[[i]], api_function,
                                        retry_count, pb_id, ...)

    for (item in names(batch_results)) {
      cache <- add_to_cache(cache, item, batch_results[[item]])
      cache_results[[item]] <- batch_results[[item]]
    }

    if (i %% save_every_n_batches == 0L || i == length(batches)) {
      save_cache(cache, cache_name, cache_dir)
    }

    if (i < length(batches) && batch_delay > 0) {
      Sys.sleep(batch_delay)
    }
  }

  cli::cli_progress_done(id = pb_id)
  cli::cli_alert_success(
    "Processed {length(items_to_process)} new {process_type} ({cached_count} from cache)"
  )

  .flatten_results(cache_results, items)
}


#' Partition items into cached and to-process, advancing progress bar
#' @keywords internal
#' @noRd
.partition_items_by_cache <- function(items, cache, max_age_days, pb_id) {
  total_items <- length(items)
  items_to_process <- character(total_items)
  cache_results <- vector("list", total_items)
  names(cache_results) <- items

  process_idx <- 0L
  cached_count <- 0L

  for (i in seq_along(items)) {
    item <- items[i]
    cached_value <- get_from_cache(cache, item, default = NULL,
                                   max_age_days = max_age_days)

    if (is.null(cached_value)) {
      process_idx <- process_idx + 1L
      items_to_process[process_idx] <- item
    } else {
      cache_results[[item]] <- cached_value
      cached_count <- cached_count + 1L
      cli::cli_progress_update(id = pb_id, inc = 1L)
    }
  }

  list(
    cache_results    = cache_results,
    items_to_process = items_to_process[seq_len(process_idx)],
    cached_count     = cached_count
  )
}


#' Process a single batch with retry logic
#' @keywords internal
#' @noRd
.process_one_batch <- function(batch, api_function, retry_count, pb_id, ...) {
  batch_results <- list()

  for (item in batch) {
    result <- NULL
    attempts <- 0L

    while (is.null(result) && attempts < retry_count) {
      attempts <- attempts + 1L
      tryCatch({
        result <- api_function(item, ...)
        if (!is.null(result)) {
          batch_results[[item]] <- result
        }
      }, error = function(e) {
        if (attempts == retry_count) {
          cli::cli_alert_warning(
            "Failed to process {.strong {item}} after {retry_count} attempts: {e$message}"
          )
          batch_results[[item]] <<- NA
        } else {
          Sys.sleep(1)
        }
      })
    }

    if (is.null(result)) {
      batch_results[[item]] <- NA
    }

    cli::cli_progress_update(id = pb_id, inc = 1L)
  }

  batch_results
}


#' Coerce per-item cache results into a named character vector
#'
#' Some api_functions return single-element lists; this collapses them and
#' guarantees a flat character vector preserving the requested item order.
#' @keywords internal
#' @noRd
.flatten_results <- function(cache_results, items) {
  vapply(cache_results[items], function(x) {
    if (is.list(x) && length(x) == 1L) {
      as.character(x[[1]])
    } else {
      as.character(x)
    }
  }, FUN.VALUE = character(1), USE.NAMES = TRUE)
}
