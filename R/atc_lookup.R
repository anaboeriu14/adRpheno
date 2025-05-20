#' Get ATC Level 2 Classifications for RxCUIs
#'
#' Retrieves second level Anatomical Therapeutic Chemical (ATC) classifications for medications
#' based on their RxCUI codes. Uses efficient caching and API batching.
#'
#' @param dataf A data frame containing RxCUI values
#' @param rxcui_col Character. Column containing RxCUI values
#' @param new_col_name Character. Name for the output column (default: "atc_classification")
#' @param include_codes Logical. Include ATC codes in output (default: FALSE)
#' @param code_col_suffix Character. Suffix for ATC code column (default: "_code")
#' @param batch_size Numeric. Batch size for API calls (default: 100)
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param save_freq Numeric. Cache save frequency (default: 50)
#' @param force_reprocess Logical. Force reprocessing (default: FALSE)
#'
#' @return Data frame with ATC level 2 classifications added (semicolon-separated for multiple values)
#' @export
get_atc_level2 <- function(dataf,
                           rxcui_col,
                           new_col_name = "atc_classification",
                           include_codes = FALSE,
                           code_col_suffix = "_code",
                           batch_size = 500,
                           cache_dir = "cache",
                           save_freq = 100,
                           force_reprocess = FALSE) {

  # Validate inputs
  if (!is.data.frame(dataf)) stop("'dataf' must be a data frame")
  if (!rxcui_col %in% names(dataf)) stop(paste("Column", rxcui_col, "not found in dataf"))

  # Set cache name specifically for level 2
  cache_name <- "rxcui_atc_level2"

  # Determine column names
  if (include_codes) {
    code_col <- paste0(new_col_name, code_col_suffix)
    if (code_col %in% names(dataf)) {
      warning("Column '", code_col, "' already exists and will be overwritten.")
    }
  }

  # Check if already processed
  tracking_id <- paste0(rxcui_col, "_atc_level2")
  if (!force_reprocess && adRutils::is_processed("get_atc_level2", tracking_id)) {
    message("Column '", rxcui_col, "' already processed for ATC level 2. Use force_reprocess=TRUE to override.")
    return(dataf)
  }

  # Initialize cache
  cache <- adRutils::initialize_cache(cache_name, cache_dir)

  # Also initialize a codes cache if needed
  if (include_codes) {
    code_cache <- adRutils::initialize_cache(paste0(cache_name, "_codes"), cache_dir)
  }

  # Extract unique, non-NA RxCUIs
  rxcuis <- dataf[[rxcui_col]]
  unique_rxcuis <- unique(rxcuis[!is.na(rxcuis) & rxcuis != ""])

  if (length(unique_rxcuis) == 0) {
    message("No valid RxCUIs found in the dataf")
    dataf[[new_col_name]] <- NA_character_
    if (include_codes) {
      dataf[[code_col]] <- NA_character_
    }
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
    message("Fetching ATC level 2 classifications for ", length(rxcuis_to_fetch), " RxCUIs in ", num_batches, " batches")

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

        # Get ATC classifications
        tryCatch({
          # Make API call to RxClass for this RxCUI
          base_url <- "https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json"
          url <- paste0(base_url, "?rxcui=", rxcui, "&relaSource=ATC")

          response <- try(httr::GET(url), silent = TRUE)

          # If successful, process the response
          if (!inherits(response, "try-error") && httr::status_code(response) == 200) {
            json_resp <- try(httr::content(response, "parsed", encoding = "UTF-8"), silent = TRUE)

            if (!inherits(json_resp, "try-error") && !is.null(json_resp) &&
                !is.null(json_resp$rxclassDrugInfoList) &&
                !is.null(json_resp$rxclassDrugInfoList$rxclassDrugInfo)) {

              # Get drug info list
              drug_info_list <- json_resp$rxclassDrugInfoList$rxclassDrugInfo

              # Filter for ATC classes
              atc_info <- Filter(function(x) {
                !is.null(x$rxclassMinConceptItem$classType) &&
                  x$rxclassMinConceptItem$classType == "ATC" &&
                  !is.null(x$rxclassMinConceptItem$classId)
              }, drug_info_list)

              if (length(atc_info) > 0) {
                # Extract ATC codes
                all_atc_codes <- sapply(atc_info, function(x) x$rxclassMinConceptItem$classId)

                # Get the level 2 ATC codes (first 3 characters)
                level2_codes <- character(0)
                for (atc_code in all_atc_codes) {
                  if (nchar(atc_code) >= 3) {
                    level2_code <- substr(atc_code, 1, 3)
                    if (!(level2_code %in% level2_codes)) {
                      level2_codes <- c(level2_codes, level2_code)
                    }
                  }
                }

                # Get the level 2 classification names
                level2_names <- character(0)

                for (level2_code in level2_codes) {
                  # Make API call to get the level 2 classification name
                  level2_url <- paste0(
                    "https://rxnav.nlm.nih.gov/REST/rxclass/classById.json",
                    "?classId=", utils::URLencode(level2_code),
                    "&relaSource=ATC"
                  )

                  level2_response <- try(httr::GET(level2_url), silent = TRUE)

                  if (!inherits(level2_response, "try-error") &&
                      httr::status_code(level2_response) == 200) {

                    level2_json <- try(httr::content(level2_response, "parsed", encoding = "UTF-8"), silent = TRUE)

                    if (!inherits(level2_json, "try-error") && !is.null(level2_json) &&
                        !is.null(level2_json$rxclassMinConceptList) &&
                        !is.null(level2_json$rxclassMinConceptList$rxclassMinConcept) &&
                        length(level2_json$rxclassMinConceptList$rxclassMinConcept) > 0 &&
                        !is.null(level2_json$rxclassMinConceptList$rxclassMinConcept[[1]]$className)) {

                      class_name <- level2_json$rxclassMinConceptList$rxclassMinConcept[[1]]$className
                      level2_names <- c(level2_names, class_name)
                    } else {
                      # If we can't get the name, use the code as fallback
                      level2_names <- c(level2_names, level2_code)
                    }
                  } else {
                    # If API call fails, use the code as fallback
                    level2_names <- c(level2_names, level2_code)
                  }

                  # Add small delay between API calls
                  Sys.sleep(0.05)
                }

                # Add results to cache
                cache <- adRutils::add_to_cache(cache, rxcui, level2_names)

                if (include_codes) {
                  code_cache <- adRutils::add_to_cache(code_cache, rxcui, level2_codes)
                }
              } else {
                # No ATC codes found
                cache <- adRutils::add_to_cache(cache, rxcui, NA_character_)

                if (include_codes) {
                  code_cache <- adRutils::add_to_cache(code_cache, rxcui, NA_character_)
                }
              }
            }
          }

          # Save cache periodically
          if (j %% save_freq == 0 || j == length(batch_rxcuis)) {
            adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE)

            if (include_codes) {
              adRutils::save_cache(code_cache, paste0(cache_name, "_codes"), cache_dir, force = TRUE)
            }
          }

          Sys.sleep(0.1)  # Small delay between RxCUIs
        },
        error = function(e) {
          message("\nError fetching ATC for '", rxcui, "': ", e$message)
        })
      }

      close(pb)
    }

    # Final cache save
    adRutils::save_cache(cache, cache_name, cache_dir, force = TRUE)

    if (include_codes) {
      adRutils::save_cache(code_cache, paste0(cache_name, "_codes"), cache_dir, force = TRUE)
    }
  } else {
    message("All RxCUIs found in cache")
  }

  # Apply ATC classifications to dataframe
  message("Adding ATC level 2 classifications to dataframe")
  result_df <- dataf
  result_df[[new_col_name]] <- NA_character_

  if (include_codes) {
    result_df[[code_col]] <- NA_character_
  }

  for (i in 1:nrow(result_df)) {
    rxcui <- result_df[[rxcui_col]][i]
    if (!is.na(rxcui) && rxcui != "") {
      atc_names <- adRutils::get_from_cache(cache, as.character(rxcui))

      if (!is.null(atc_names) && length(atc_names) > 0 && !all(is.na(atc_names))) {
        result_df[[new_col_name]][i] <- paste(atc_names, collapse = ";")

        # Add codes if requested
        if (include_codes) {
          atc_codes <- adRutils::get_from_cache(code_cache, as.character(rxcui))

          if (!is.null(atc_codes) && length(atc_codes) > 0 && !all(is.na(atc_codes))) {
            result_df[[code_col]][i] <- paste(atc_codes, collapse = ";")
          }
        }
      }
    }
  }

  # Register as processed
  adRutils::register_processed("get_atc_level2", tracking_id)

  # Report results
  atc_found_count <- sum(!is.na(result_df[[new_col_name]]))
  valid_rxcui_count <- sum(!is.na(dataf[[rxcui_col]]))

  if (valid_rxcui_count > 0) {
    message("Found ATC level 2 classifications for ", atc_found_count, "/", valid_rxcui_count,
            " RxCUIs (", round(atc_found_count/valid_rxcui_count*100, 1), "% coverage)")
  }

  return(result_df)
}

#' Get ATC Level 2 Classifications with unnested results
#'
#' Same as get_atc_level2 but returns unnested results (one row per RxCUI-ATC pair).
#' Requires dplyr and tidyr packages.
#'
#' @inheritParams get_atc_level2
#' @return Data frame with ATC level 2 classifications (one row per medication-ATC pair)
#' @export
get_atc_level2_unnested <- function(dataf,
                                    rxcui_col,
                                    new_col_name = "atc_classification",
                                    include_codes = FALSE,
                                    code_col_suffix = "_code",
                                    batch_size = 500,
                                    cache_dir = "cache",
                                    save_freq = 100,
                                    force_reprocess = FALSE) {

  # Check for required packages
  if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'tidyr' and 'dplyr' are required for unnested results")
  }

  # Set code column
  code_col <- paste0(new_col_name, code_col_suffix)

  # First get the ATC classifications
  result_df <- get_atc_level2(
    dataf = dataf,
    rxcui_col = rxcui_col,
    new_col_name = "temp_atc",
    include_codes = include_codes,
    code_col_suffix = code_col_suffix,
    batch_size = batch_size,
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

  # Handle codes
  if (include_codes) {
    temp_code_col <- paste0("temp_atc", code_col_suffix)
    temp_code_sym <- rlang::sym(temp_code_col)
    code_col_sym <- rlang::sym(code_col)

    # Unnest both columns simultaneously
    unnested_df <- result_df %>%
      tidyr::separate_rows(!!temp_atc_sym, !!temp_code_sym, sep = ";") %>%
      dplyr::rename(!!new_col_sym := !!temp_atc_sym,
                    !!code_col_sym := !!temp_code_sym) %>%
      dplyr::select(-".row_id")
  } else {
    # Just unnest the classifications
    unnested_df <- result_df %>%
      tidyr::separate_rows(!!temp_atc_sym, sep = ";") %>%
      dplyr::rename(!!new_col_sym := !!temp_atc_sym) %>%
      dplyr::select(-".row_id")
  }

  return(unnested_df)
}
