#' Process items in batches with caching
#'
#' @param items Items to process
#' @param api_function Function to call for each item
#' @param batch_size Items per batch (default: 200)
#' @param save_freq Save cache frequency (default: 50)
#' @param cache_name Cache name
#' @param cache_dir Cache directory
#' @param max_age_days Cache expiration (default: 30)
#' @param retry_count Retry attempts (default: 3)
#' @param batch_delay Delay between batches (default: 2)
#' @param process_type Label for progress bar
#' @param ... Arguments passed to api_function
#'
#' @return Named vector of results
#' @keywords internal
#' @noRd
.process_batch <- function(items,
                           api_function,
                           batch_size = 200,
                           save_freq = 50,
                           cache_name = "api_cache",
                           cache_dir = "cache",
                           max_age_days = 30,
                           retry_count = 3,
                           batch_delay = 2,
                           process_type = "items",
                           ...) {

  cache <- .load_cache(cache_name, cache_dir, max_age_days)

  # Split cached vs new
  split_items <- .split_cached_items(items, cache, max_age_days)

  if (length(split_items$to_process) == 0) {
    return(.format_results(split_items$cached, items))
  }

  # Process new items
  results <- split_items$cached
  batches <- .create_batches(split_items$to_process, batch_size)

  cli::cli_progress_bar(
    format = "{process_type}: [{cli::pb_bar}] {cli::pb_percent}",
    total = length(split_items$to_process)
  )

  for (i in seq_along(batches)) {
    batch_results <- .process_single_batch(batches[[i]], api_function, retry_count, ...)

    for (item in names(batch_results)) {
      cache <- adRutils::add_to_cache(cache, item, batch_results[[item]])
      results[[item]] <- batch_results[[item]]
      cli::cli_progress_update()
    }

    # Periodic save
    if (i %% max(1, ceiling(save_freq / batch_size)) == 0 || i == length(batches)) {
      adRutils::save_cache(cache, cache_name, cache_dir)
    }

    if (i < length(batches) && batch_delay > 0) Sys.sleep(batch_delay)
  }

  cli::cli_progress_done()

  .format_results(results, items)
}


#' @keywords internal
#' @noRd
.load_cache <- function(cache_name, cache_dir, max_age_days) {
  cache <- adRutils::initialize_cache(cache_name, cache_dir)

  if (adRutils::is_cache_expired(cache, max_age_days)) {
    cache <- adRutils::clean_cache(cache, max_age_days)
  }

  cache
}


#' @keywords internal
#' @noRd
.split_cached_items <- function(items, cache, max_age_days) {
  cached <- list()
  to_process <- character()

  for (item in items) {
    val <- adRutils::get_from_cache(cache, item, default = NULL, max_age_days = max_age_days)
    if (is.null(val)) {
      to_process <- c(to_process, item)
    } else {
      cached[[item]] <- val
    }
  }

  list(cached = cached, to_process = to_process)
}


#' @keywords internal
#' @noRd
.create_batches <- function(items, batch_size) {
  split(items, ceiling(seq_along(items) / batch_size))
}


#' @keywords internal
#' @noRd
.process_single_batch <- function(batch, api_function, retry_count, ...) {
  results <- list()

  for (item in batch) {
    result <- .call_with_retry(api_function, item, retry_count, ...)
    results[[item]] <- result
  }

  results
}


#' @keywords internal
#' @noRd
.call_with_retry <- function(fn, item, retry_count, ...) {
  for (attempt in seq_len(retry_count)) {
    result <- tryCatch(fn(item, ...), error = function(e) NULL)
    if (!is.null(result)) return(result)
    if (attempt < retry_count) Sys.sleep(0.5)
  }
  NA
}


#' @keywords internal
#' @noRd
.format_results <- function(results, items) {
  sapply(results[items], function(x) {
    if (is.list(x) && length(x) == 1) {
      as.character(x[[1]])
    } else if (is.null(x) || (length(x) == 1 && is.na(x))) {
      NA_character_
    } else {
      as.character(x)
    }
  })
}
