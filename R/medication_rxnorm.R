#' Get standardized drug names from RxNorm API
#'
#' This function takes a vector of medication names and returns standardized
#' drug names by querying the RxNorm API. Results are cached to avoid
#' redundant API calls and variables are tracked to prevent redundant processing.
#'
#' @param medicine_names A character vector of medication names to standardize
#' @param max_entries Maximum number of entries to return from the API (default: 1)
#' @param match_option Match option for the API query (default: 1 for exact match)
#' @param cache_name File name for the cache (default: "rxnorm_names_cache")
#' @param cache_dir Directory name for the cache (default: "cache")
#' @param save_freq Frequency of how often to save cache (default: 100)
#' @param max_age_days Maximum age of cached entries in days (default: 90)
#' @param track_processing Logical indicating whether to track processed variables (default: TRUE)
#' @param reset_track Logical indicating whether to reset tracking for this function (default: FALSE)
#' @param retry_count Number of times to retry failed API calls (default: 3)
#' @param retry_delay Seconds to wait between retries (default: 1)
#'
#' @return A character vector of standardized medication names
#' @export
get_rxnorm_medication_names <- function(medicine_names, max_entries = 1, match_option = 1,
                                  cache_name = "rxnorm_names_cache", cache_dir = "cache",
                                  save_freq = 100, max_age_days = 90,
                                  track_processing = TRUE, reset_track = FALSE,
                                  retry_count = 3, retry_delay = 1) {

  # Reset tracking if requested
  if (reset_track) {
    reset_processing("get_rxnorm_medication_names")
  }

  # Input validation with detailed error messages
  if (!is.character(medicine_names)) {
    stop("'medicine_names' must be a character vector.")
  }

  if (!is.numeric(max_entries) || max_entries < 1) {
    stop("'max_entries' must be a positive integer.")
  }

  if (!is.numeric(match_option) || !(match_option %in% c(0, 1, 2))) {
    stop("'match_option' must be 0 (all), 1 (exact), or 2 (contains).")
  }

  # Generate a unique identifier for this set of parameters
  param_key <- sprintf("%d_%d_%d", max_entries, match_option, max_age_days)

  # Initialize cache with proper extension
  cache <- adRutils::initialize_cache(cache_name, cache_dir, ".rds")

  # Process unique medicine names to reduce API calls
  medicine_names_clean <- trimws(medicine_names)
  unique_meds <- unique(medicine_names_clean[!is.na(medicine_names_clean)])
  lookup_indices <- match(medicine_names_clean, unique_meds)
  results_names <- character(length(medicine_names_clean))

  # Keep track of new cache entries for periodic saving
  new_entries_count <- 0

  # Check if cache needs cleaning (do this only once per function call)
  cache <- adRutils::clean_cache(cache, max_age_days)

  # Create a more descriptive progress bar
  message("Standardizing ", length(unique_meds), " unique medication names")
  prog_bar <- utils::txtProgressBar(min = 0, max = length(unique_meds), style = 3)

  # Process each unique medication
  for (i in seq_along(unique_meds)) {
    med_name <- unique_meds[i]

    # Skip if this medication name has already been processed by this function
    # (with these parameters) and tracking is enabled
    if (track_processing) {
      track_key <- paste0(med_name, "_", param_key)
      already_processed <- adRutils::is_processed("get_rxnorm_medication_names", track_key)
      if (already_processed) {
        # get the result from cache
        cache_key <- paste0(med_name, "_", max_entries, "_", match_option)
        cached_result <- adRutils::get_from_cache(cache, cache_key, default = NULL,
                                                  max_age_days = max_age_days)

        if (!is.null(cached_result)) {
          # Update the lookup for this medication
          indices <- which(unique_meds[lookup_indices] == med_name)
          results_names[indices] <- cached_result
          setTxtProgressBar(prog_bar, i)
          next
        }
      }
    }

    # Prepare cache key
    cache_key <- paste0(med_name, "_", max_entries, "_", match_option)

    # Check cache first
    cached_result <- adRutils::get_from_cache(cache, cache_key,
                                              default = NULL, max_age_days = max_age_days)

    if (!is.null(cached_result)) {
      result <- cached_result
    } else {
      # Not in cache or expired, make API call with retries
      result <- med_name  # Default to original name

      for (attempt in 1:retry_count) {
        # Simple rate limiting to avoid overwhelming the API
        Sys.sleep(0.1)

        response <- tryCatch({
          httr::GET(
            "https://rxnav.nlm.nih.gov/REST/approximateTerm.json",
            query = list(
              term = med_name,
              maxEntries = max_entries,
              option = match_option
            ),
            httr::timeout(5)  # Add timeout to prevent hanging
          )
        }, error = function(e) {
          message("API request failed for '", med_name, "': ", e$message)
          NULL
        })

        # Process response if successful
        if (!is.null(response) && httr::status_code(response) == 200) {
          content_text <- httr::content(response, "text", encoding = "UTF-8")

          # Ensure we got valid JSON
          valid_json <- tryCatch({
            parsed_data <- jsonlite::fromJSON(content_text, flatten = TRUE)
            TRUE
          }, error = function(e) {
            message("Failed to parse JSON for '", med_name, "': ", e$message)
            FALSE
          })

          if (valid_json) {
            parsed_data <- jsonlite::fromJSON(content_text, flatten = TRUE)

            # Check if we have candidates
            if (!is.null(parsed_data$approximateGroup$candidate)) {
              candidates <- parsed_data$approximateGroup$candidate

              if (length(candidates) > 0) {
                # Get the first valid name (or RxCUI if available)
                valid_indices <- which(!is.na(candidates$name) & candidates$name != "")

                if (length(valid_indices) > 0) {
                  first_valid_idx <- valid_indices[1]
                  result <- candidates$name[first_valid_idx]
                }
              }
            }
          }
        }

        # If we get here and it's not the last attempt, wait and retry
        if (attempt < retry_count) {
          message("Retrying API call for '", med_name, "' (attempt ", attempt + 1, " of ", retry_count, ")")
          Sys.sleep(retry_delay)
        }
      }

      # Add to cache
      cache <- adRutils::add_to_cache(cache, cache_key, result)
      new_entries_count <- new_entries_count + 1

      # Save cache periodically or when finishing
      if (new_entries_count %% save_freq == 0 || i == length(unique_meds)) {
        adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE, compress = TRUE)
        message("\nSaved cache with ", new_entries_count, " new entries")
        new_entries_count <- 0  # Reset counter after saving
      }

      # Register as processed if tracking is enabled
      if (track_processing) {
        track_key <- paste0(med_name, "_", param_key)
        adRutils::register_processed("get_rxnorm_medication_names", track_key)
      }
    }

    # Update progress bar
    setTxtProgressBar(prog_bar, i)
  }

  close(prog_bar)

  # Map results back to original indices
  for (i in seq_along(medicine_names_clean)) {
    if (is.na(medicine_names_clean[i])) {
      results_names[i] <- NA_character_
    } else {
      unique_idx <- lookup_indices[i]
      med_name <- unique_meds[unique_idx]
      cache_key <- paste0(med_name, "_", max_entries, "_", match_option)
      results_names[i] <- adRutils::get_from_cache(cache, cache_key, default = med_name)
    }
  }

  # Output summary statistics
  cache_stats <- adRutils::get_cache_stats(cache)
  message(sprintf("\nCache statistics: %d entries, %.1f days old",
                  cache_stats$entry_count,
                  if(is.na(cache_stats$age_days)) 0 else cache_stats$age_days))

  # Return the corrected names, preserving the original vector structure
  return(results_names)
}


#' Apply RxNorm standardization to a data frame column
#'
#' This function takes a data frame and adds a new column with corrected
#' medication names by applying the get_rxnorm_medication_names function to a specified column.
#'
#' @param dataf A data frame containing medication names
#' @param column_name Name of the column containing medication names to standardize
#' @param new_column_name Name of the new column to create (default: "api_corrected")
#' @param ... Additional arguments passed to get_rxnorm_medication_names(), such as:
#'   \itemize{
#'     \item max_entries: Maximum number of entries to return from the API (default: 1)
#'     \item match_option: Match option for the API query (default: 1 for exact match)
#'     \item cache_name: File name for the cache (default: "rxnorm_names_cache")
#'     \item And others, see ?get_rxnorm_medication_names for details
#'   }
#'
#' @return The data frame with an additional column containing standardized names
#' @export
correct_medication_names_df <- function(dataf, column_name, new_column_name = "api_corrected", ...) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("'dataf' must be a data frame.")
  }

  if (!column_name %in% names(dataf)) {
    stop(sprintf("Column '%s' not found in the data frame.", column_name))
  }

  # Add the new column directly, without creating an intermediate variable
  dataf[[new_column_name]] <- get_rxnorm_medication_names(dataf[[column_name]], ...)

  # Return the enhanced data frame
  return(dataf)
}

