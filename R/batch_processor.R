#' Process items in batches with caching, retry, and progress reporting
#'
#' Internal orchestration helper that wraps an `api_function` (one call per
#' item) with on-disk caching (`.initialize_cache()`), batched
#' processing, retry-on-failure with exponential backoff, and a `cli`
#' progress bar.
#'
#' Retries: failed calls are retried up to `retry_count` times with backoff
#' `0.5 * 2^(attempt - 1)` seconds (0.5s, 1s, 2s, ...). `api_function` should
#' raise an error or return `NULL` to signal failure.
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
#' @return Named list of results, one entry per input item. List shape
#'   (rather than character vector) lets `api_function` return scalars,
#'   vectors, or `NULL` per item; callers flatten to their preferred shape.
#' @keywords internal
#' @noRd
.process_batch <- function(items, api_function, batch_size = 200, save_freq = 50,
                           cache_name = "api_cache", cache_dir = "cache",
                           max_age_days = 30, retry_count = 3, batch_delay = 2,
                           process_type = "items", ...) {

  cache <- .initialize_cache(cache_name, cache_dir)
  if (is_cache_expired(cache, max_age_days)) {
    cache <- .clean_cache(cache, max_age_days)
  }

  total_items <- length(items)

  pb_id <- cli::cli_progress_bar(
    format = paste0("Processing ", process_type,
                    " [{cli::pb_bar}] {cli::pb_current}/{cli::pb_total}"),
    total  = total_items,
    clear  = FALSE
  )

  partition <- .partition_items_by_cache(items, cache, max_age_days, pb_id)
  cache_results    <- partition$cache_results
  items_to_process <- partition$items_to_process
  cached_count     <- partition$cached_count

  if (length(items_to_process) == 0L) {
    cli::cli_progress_done(id = pb_id)
    cli::cli_alert_success("All {total_items} {process_type} retrieved from cache")
    return(cache_results[items])
  }

  if (cached_count > 0L) {
    cli::cli_alert_info("{cached_count}/{total_items} {process_type} found in cache")
  }

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
      cache <- .add_to_cache(cache, item, batch_results[[item]])
      cache_results[[item]] <- batch_results[[item]]
    }

    if (i %% save_every_n_batches == 0L || i == length(batches)) {
      .save_cache(cache, cache_name, cache_dir)
    }

    if (i < length(batches) && batch_delay > 0) {
      Sys.sleep(batch_delay)
    }
  }

  cli::cli_progress_done(id = pb_id)
  cli::cli_alert_success(
    "Processed {length(items_to_process)} new {process_type} ({cached_count} from cache)"
  )

  cache_results[items]
}


# ---- internal helpers ------------------------------------------------------

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
    cached_value <- .get_from_cache(cache, item, default = NULL,
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


#' Process a single batch with retry-on-error and exponential backoff
#'
#' For each item, attempts up to `retry_count` calls. On failure, waits
#' `0.5 * 2^(attempt - 1)` seconds before retrying. After exhausting retries,
#' stores `NA` for that item and emits a warning.
#' @keywords internal
#' @noRd
.process_one_batch <- function(batch, api_function, retry_count, pb_id, ...) {
  batch_results <- list()

  for (item in batch) {
    batch_results[[item]] <- .call_with_retry(api_function, item, retry_count, ...)
    cli::cli_progress_update(id = pb_id, inc = 1L)
  }

  batch_results
}


#' Call `api_function(item, ...)` with bounded retries and exponential backoff
#' @keywords internal
#' @noRd
.call_with_retry <- function(api_function, item, retry_count, ...) {
  for (attempt in seq_len(retry_count)) {
    result <- tryCatch(
      api_function(item, ...),
      error = function(e) {
        if (attempt == retry_count) {
          cli::cli_alert_warning(
            "Failed to process {.strong {item}} after {retry_count} attempts: {e$message}"
          )
        }
        NULL
      }
    )

    if (!is.null(result)) return(result)

    if (attempt < retry_count) {
      Sys.sleep(0.5 * 2^(attempt - 1L))
    }
  }

  NA
}
