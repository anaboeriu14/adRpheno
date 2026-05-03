#' Initialize a cache object
#'
#' Loads an existing on-disk cache or creates a new in-memory one. If the
#' on-disk file exists but is unreadable or has the wrong shape, a warning is
#' issued and a fresh cache is returned.
#'
#' @param cache_name Name for the cache (used as filename stem).
#' @param cache_dir Directory for cache files. Default `"cache"`.
#'
#' @return A cache object: a list with `name`, `created`, and `entries`.
#' @keywords internal
#' @noRd
.initialize_cache <- function(cache_name, cache_dir = "cache") {
  cache_path <- file.path(cache_dir, paste0(cache_name, ".rds"))

  if (file.exists(cache_path)) {
    cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (.is_valid_cache(cache)) return(cache)
    cli::cli_alert_warning(
      "Cache at {.path {cache_path}} is unreadable or malformed; reinitializing"
    )
  }

  list(
    name    = cache_name,
    created = Sys.time(),
    entries = list()
  )
}


#' Add an entry to a cache
#'
#' Stores `value` under `key` in the cache, with the current timestamp.
#' This does not persist to disk; call `save_cache()` to persist.
#'
#' @param cache A cache object from `initialize_cache()`.
#' @param key Cache key (coerced to character).
#' @param value Value to store.
#'
#' @return The updated cache object.
#' @keywords internal
#' @noRd
.add_to_cache <- function(cache, key, value) {
  cache$entries[[as.character(key)]] <- list(
    value     = value,
    timestamp = Sys.time()
  )
  cache
}


#' Retrieve an entry from a cache
#'
#' Returns the cached value if present and not expired, otherwise `default`.
#' Expiration is checked per-entry, not for the cache as a whole.
#'
#' @param cache A cache object.
#' @param key Cache key.
#' @param default Value returned when the key is absent or expired.
#'   Default `NULL`.
#' @param max_age_days Maximum entry age in days. Default `30`.
#'
#' @return The cached value, or `default`.
#' @keywords internal
#' @noRd
.get_from_cache <- function(cache, key, default = NULL, max_age_days = 30) {
  entry <- cache$entries[[as.character(key)]]
  if (is.null(entry)) return(default)

  age_days <- as.numeric(difftime(Sys.time(), entry$timestamp, units = "days"))
  if (age_days > max_age_days) return(default)

  entry$value
}


#' Persist a cache to disk
#'
#' Writes the cache atomically: serializes to a temporary file in the same
#' directory, then renames into place. This avoids leaving a corrupted file
#' if the process is interrupted mid-write.
#'
#' @param cache A cache object.
#' @param cache_name Name for the cache (used as filename stem).
#' @param cache_dir Directory for cache files. Default `"cache"`.
#'
#' @return Invisibly, the file path written.
#' @keywords internal
#' @noRd
save_cache <- function(cache, cache_name, cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_path <- file.path(cache_dir, paste0(cache_name, ".rds"))
  tmp_path   <- paste0(cache_path, ".tmp")

  saveRDS(cache, tmp_path)
  # file.rename is atomic on POSIX filesystems for same-directory moves
  ok <- file.rename(tmp_path, cache_path)
  if (!ok) {
    # Fallback: try copy + remove (Windows can fail rename if target exists)
    file.copy(tmp_path, cache_path, overwrite = TRUE)
    file.remove(tmp_path)
  }

  invisible(cache_path)
}


#' Remove expired entries from a cache
#'
#' @param cache A cache object.
#' @param max_age_days Maximum entry age in days. Default `30`.
#'
#' @return The cache object with expired entries removed.
#' @keywords internal
#' @noRd
clean_cache <- function(cache, max_age_days = 30) {
  now <- Sys.time()
  cache$entries <- Filter(function(entry) {
    age_days <- as.numeric(difftime(now, entry$timestamp, units = "days"))
    age_days <= max_age_days
  }, cache$entries)
  cache
}


#' Check whether a cache as a whole is past its expiration window
#'
#' Compares the cache's `created` timestamp against `max_age_days`. Useful as
#' a cheap "should I bother cleaning this?" check before iterating entries.
#'
#' @param cache A cache object.
#' @param max_age_days Maximum cache age in days. Default `30`.
#'
#' @return Logical scalar. `TRUE` if the cache is older than `max_age_days`
#'   (or has no `created` timestamp), `FALSE` otherwise.
#' @keywords internal
#' @noRd
is_cache_expired <- function(cache, max_age_days = 30) {
  if (is.null(cache$created)) return(TRUE)
  age_days <- as.numeric(difftime(Sys.time(), cache$created, units = "days"))
  age_days > max_age_days
}


#' Summarize cache contents
#'
#' @param cache A cache object.
#'
#' @return A list with `entry_count`, `created`, and `age_days`.
#' @keywords internal
#' @noRd
get_cache_status <- function(cache) {
  list(
    entry_count = length(cache$entries),
    created     = cache$created,
    age_days    = as.numeric(difftime(Sys.time(), cache$created, units = "days"))
  )
}


#' Validate the structural shape of a cache object
#' @keywords internal
#' @noRd
.is_valid_cache <- function(x) {
  is.list(x) &&
    all(c("name", "created", "entries") %in% names(x)) &&
    is.list(x$entries)
}
