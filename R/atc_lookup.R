#' Get ATC Classifications for RxCUIs
#'
#' Retrieves Anatomical Therapeutic Chemical (ATC) classifications for medications based on their
#' RxCUI codes. Uses efficient caching and API batching.
#'
#' @param dataf A data frame containing RxCUI values
#' @param rxcui_col Character. Column containing RxCUI values
#' @param atc_group Character or numeric. ATC group level (1-5 or "first" through "fifth")
#' @param new_col_name Character. Name for the output column (default: "atc_code")
#' @param batch_size Numeric. Batch size for API calls (default: 100)
#' @param cache_name Character. Custom cache name (default: based on ATC group)
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param save_freq Numeric. Cache save frequency (default: 50)
#' @param force_reprocess Logical. Force reprocessing (default: FALSE)
#'
#' @return Data frame with ATC codes added (semicolon-separated for multiple values)
#' @export
get_atc_codes <- function(dataf,
                          rxcui_col,
                          atc_group,
                          new_col_name = "atc_code",
                          batch_size = 500,
                          cache_name = NULL,
                          cache_dir = "cache",
                          save_freq = 100,
                          force_reprocess = FALSE) {

  # Validate inputs
  if (!is.data.frame(dataf)) stop("'dataf' must be a data frame")
  if (!rxcui_col %in% names(dataf)) stop(paste("Column", rxcui_col, "not found in dataf"))

  # Validate ATC group
  valid_text_groups <- c("first", "second", "third", "fourth", "fifth")
  valid_numeric_groups <- 1:5

  if (is.character(atc_group)) {
    if (!atc_group %in% valid_text_groups) {
      stop("atc_group must be one of: 'first', 'second', 'third', 'fourth', 'fifth'")
    }
  } else if (is.numeric(atc_group)) {
    if (!atc_group %in% valid_numeric_groups) {
      stop("numeric atc_group must be between 1 and 5")
    }
  } else {
    stop("atc_group must be a character string or numeric value")
  }

  # Create automatic cache name if not provided
  if (is.null(cache_name)) {
    cache_name <- paste0("rxcui_atc_", as.character(atc_group))
  }

  # Check if already processed
  tracking_id <- paste0(rxcui_col, "_atc_", atc_group)
  if (!force_reprocess && adRutils::is_processed("get_atc_codes", tracking_id)) {
    message("Column '", rxcui_col, "' already processed for ATC level ",
            atc_group, ". Use force_reprocess=TRUE to override.")
    return(dataf)
  }

  # Initialize cache
  cache <- adRutils::initialize_cache(cache_name, cache_dir)

  # Extract unique, non-NA RxCUIs
  rxcuis <- dataf[[rxcui_col]]
  unique_rxcuis <- unique(rxcuis[!is.na(rxcuis) & rxcuis != ""])

  if (length(unique_rxcuis) == 0) {
    message("No valid RxCUIs found in the dataf")
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # Find RxCUIs not in cache
  rxcuis_to_fetch <- character(0)
  for (rxcui in unique_rxcuis) {
    if (is.null(adRutils::get_from_cache(cache, as.character(rxcui)))) {
      rxcuis_to_fetch <- c(rxcuis_to_fetch, as.character(rxcui))
    }
  }

  # Fetch from API if needed
  if (length(rxcuis_to_fetch) > 0) {
    # Process in batches
    num_batches <- ceiling(length(rxcuis_to_fetch) / batch_size)
    message("Fetching ATC codes for ", length(rxcuis_to_fetch), " RxCUIs in ", num_batches, " batches")

    # Process each batch
    for (i in 1:num_batches) {
      start_idx <- (i-1) * batch_size + 1
      end_idx <- min(i * batch_size, length(rxcuis_to_fetch))
      batch_rxcuis <- rxcuis_to_fetch[start_idx:end_idx]

      message("Processing batch ", i, " of ", num_batches)
      pb <- txtProgressBar(min = 0, max = length(batch_rxcuis), style = 3)

      # Process each RxCUI
      for (j in seq_along(batch_rxcuis)) {
        rxcui <- batch_rxcuis[j]
        setTxtProgressBar(pb, j)

        # Get ATC codes
        tryCatch({
          atc_codes <- rxnorm::get_atc(rxcui, atc_group)
          cache <- adRutils::add_to_cache(cache, rxcui, atc_codes)

          # Save cache periodically
          if (j %% save_freq == 0 || j == length(batch_rxcuis)) {
            adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE)
          }

          Sys.sleep(0.1)  # Small delay
        },
        error = function(e) {
          message("\nError fetching ATC for '", rxcui, "': ", e$message)
        })
      }

      close(pb)
    }

    # Final cache save
    adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE)
  } else {
    message("All RxCUIs found in cache")
  }

  # Apply ATC codes to dataframe
  message("Adding ATC codes to dataframe")
  result_df <- dataf
  result_df[[new_col_name]] <- NA_character_

  for (i in 1:nrow(result_df)) {
    rxcui <- result_df[[rxcui_col]][i]
    if (!is.na(rxcui) && rxcui != "") {
      atc_codes <- adRutils::get_from_cache(cache, as.character(rxcui))
      if (!is.null(atc_codes) && length(atc_codes) > 0 && !all(is.na(atc_codes))) {
        result_df[[new_col_name]][i] <- paste(atc_codes, collapse = ";")
      }
    }
  }

  # Register as processed
  adRutils::register_processed("get_atc_codes", tracking_id)

  # Report results
  atc_found_count <- sum(!is.na(result_df[[new_col_name]]))
  valid_rxcui_count <- sum(!is.na(dataf[[rxcui_col]]))

  if (valid_rxcui_count > 0) {
    message("Found ATC codes for ", atc_found_count, "/", valid_rxcui_count,
            " RxCUIs (", round(atc_found_count/valid_rxcui_count*100, 1), "% coverage)")
  }

  return(result_df)
}

#' Get ATC Classifications with unnested results
#'
#' Same as get_atc_codes but returns unnested results (one row per RxCUI-ATC pair).
#' Requires dplyr and tidyr packages.
#'
#' @inheritParams get_atc_codes
#' @return Data frame with ATC codes (one row per medication-ATC pair)
#' @export
get_atc_codes_unnested <- function(dataf,
                                   rxcui_col,
                                   atc_group,
                                   new_col_name = "atc_code",
                                   batch_size = 500,
                                   cache_name = NULL,
                                   cache_dir = "cache",
                                   save_freq = 100,
                                   force_reprocess = FALSE) {

  # Check for required packages
  if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'tidyr' and 'dplyr' are required for unnested results")
  }

  # First get the ATC codes
  result_df <- get_atc_codes(
    dataf = dataf,
    rxcui_col = rxcui_col,
    atc_group = atc_group,
    new_col_name = "temp_atc",  # Temporary column name
    batch_size = batch_size,
    cache_name = cache_name,
    cache_dir = cache_dir,
    save_freq = save_freq,
    force_reprocess = force_reprocess
  )

  # Now unnest the results
  message("Unnesting results (one row per RxCUI-ATC pair)")

  # Add row ID for proper joining
  result_df <- dplyr::mutate(result_df, .row_id = dplyr::row_number())

  temp_atc_sym <- rlang::sym("temp_atc")
  new_col_sym <- rlang::sym(new_col_name)


  # Split the semicolon-separated values
  unnested_df <- result_df %>%
    tidyr::separate_rows(!!temp_atc_sym, sep = ";") %>%
    dplyr::rename(!!new_col_sym := !!temp_atc_sym) %>%
    dplyr::select(-".row_id")

  return(unnested_df)
}
