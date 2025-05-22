#' Add ATC2 classifications to medications (nested version)
#'
#' Internal function that adds ATC2 therapeutic classifications to a data frame
#' with RxCUIs. Multiple classifications for a single medication are joined
#' with semicolons.
#'
#' @param dataf A data frame containing medication information
#' @param rxcui_col Name of the column containing RxCUI values (default: "rxcui")
#' @param new_col_name Name of the new column to create (default: "atc2_class")
#' @param cache_name File name for the cache (default: "rxcui_atc2_cache")
#' @param cache_dir Directory name for the cache (default: "cache")
#'
#' @return A data frame with an additional column containing ATC2 classifications
#' @keywords internal
add_medication_atc2_nested <- function(dataf,
                                       rxcui_col = "rxcui",
                                       new_col_name = "atc2_class",
                                       cache_name = "rxcui_atc2_cache",
                                       cache_dir = "cache") {

  # Input validation
  if (!rxcui_col %in% names(dataf)) {
    stop(sprintf("Column '%s' not found in the data frame.", rxcui_col))
  }

  # Initialize cache
  cache <- adRutils::initialize_cache(cache_name, cache_dir, ".rds")

  # Extract unique RxCUIs
  unique_rxcuis <- unique(dataf[[rxcui_col]])
  unique_rxcuis <- unique_rxcuis[!is.na(unique_rxcuis)]

  if (length(unique_rxcuis) == 0) {
    warning("No valid RxCUIs found in the data frame")
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # Convert RxCUIs to character
  unique_rxcuis <- as.character(unique_rxcuis)

  # Create a progress bar
  message(paste("Getting ATC2 classifications for", length(unique_rxcuis), "unique RxCUIs"))
  pb <- utils::txtProgressBar(min = 0, max = length(unique_rxcuis), style = 3)

  # Create lookup table
  rxcui_to_atc2 <- list()
  new_cache_entries <- 0

  # Process each unique RxCUI
  for (i in seq_along(unique_rxcuis)) {
    rxcui <- unique_rxcuis[i]

    # Check cache first
    atc2_class <- adRutils::get_from_cache(cache, rxcui, default = NULL)

    if (!is.null(atc2_class)) {
      rxcui_to_atc2[[rxcui]] <- atc2_class
    } else {
      # Not in cache - get from API
      atc2_class <- tryCatch({
        # Add small delay every 20 items to avoid rate limiting
        if (i %% 20 == 0) Sys.sleep(0.2)

        # Call the rxnorm package directly with "second" level
        rxnorm::get_atc(rxcui, "second")
      }, error = function(e) {
        message("\nError for RxCUI '", rxcui, "': ", e$message)
        NA_character_
      })

      # Store result
      rxcui_to_atc2[[rxcui]] <- atc2_class

      # Add to cache
      cache <- adRutils::add_to_cache(cache, rxcui, atc2_class)
      new_cache_entries <- new_cache_entries + 1

      # Save cache occasionally
      if (new_cache_entries %% 50 == 0) {
        adRutils::save_cache(cache, cache_name, cache_dir)
        message("\nSaved ", new_cache_entries, " new cache entries")
      }
    }

    # Update progress bar
    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  # Save cache if there are new entries
  if (new_cache_entries > 0 && new_cache_entries %% 50 != 0) {
    adRutils::save_cache(cache, cache_name, cache_dir)
    message("\nSaved ", new_cache_entries, " new cache entries")
  }

  # Apply classifications to data frame
  dataf[[new_col_name]] <- NA_character_

  for (i in 1:nrow(dataf)) {
    rxcui <- dataf[[rxcui_col]][i]
    if (!is.na(rxcui)) {
      rxcui_char <- as.character(rxcui)
      if (rxcui_char %in% names(rxcui_to_atc2)) {
        atc2_class <- rxcui_to_atc2[[rxcui_char]]

        # Handle multiple classifications (join with semicolons)
        if (length(atc2_class) > 1) {
          dataf[[new_col_name]][i] <- paste(atc2_class, collapse = "; ")
        } else {
          dataf[[new_col_name]][i] <- as.character(atc2_class)
        }
      }
    }
  }

  return(dataf)
}

#' Add ATC2 classifications to medications (unnested version)
#'
#' Internal function that adds ATC2 therapeutic classifications to a data frame
#' with RxCUIs. If a medication has multiple ATC2 classifications, it creates
#' separate rows for each classification.
#'
#' @param dataf A data frame containing medication information
#' @param rxcui_col Name of the column containing RxCUI values (default: "rxcui")
#' @param new_col_name Name of the new column to create (default: "atc2_class")
#' @param cache_name File name for the cache (default: "rxcui_atc2_cache")
#' @param cache_dir Directory name for the cache (default: "cache")
#'
#' @return A data frame with an additional column containing ATC2 classifications,
#'         with one row per medication-ATC2 combination
#' @keywords internal
add_medication_atc2_unnested <- function(dataf,
                                         rxcui_col = "rxcui",
                                         new_col_name = "atc2_class",
                                         cache_name = "rxcui_atc2_cache",
                                         cache_dir = "cache") {

  # Input validation
  if (!rxcui_col %in% names(dataf)) {
    stop(sprintf("Column '%s' not found in the data frame.", rxcui_col))
  }

  # Check for required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for unnested results.")
  }

  # Initialize cache
  cache <- adRutils::initialize_cache(cache_name, cache_dir, ".rds")

  # Extract unique RxCUIs
  unique_rxcuis <- unique(dataf[[rxcui_col]])
  unique_rxcuis <- unique_rxcuis[!is.na(unique_rxcuis)]

  if (length(unique_rxcuis) == 0) {
    warning("No valid RxCUIs found in the data frame")
    dataf[[new_col_name]] <- NA_character_
    return(dataf)
  }

  # Convert RxCUIs to character
  unique_rxcuis <- as.character(unique_rxcuis)

  # Create a progress bar
  message(paste("Getting ATC2 classifications for", length(unique_rxcuis), "unique RxCUIs"))
  pb <- utils::txtProgressBar(min = 0, max = length(unique_rxcuis), style = 3)

  # Prepare a data frame to store RxCUI-to-ATC2 mappings
  atc2_mapping <- data.frame(
    rxcui = character(),
    atc2_class = character(),
    stringsAsFactors = FALSE
  )

  new_cache_entries <- 0

  # Process each unique RxCUI
  for (i in seq_along(unique_rxcuis)) {
    rxcui <- unique_rxcuis[i]

    # Check cache first
    atc2_classes <- adRutils::get_from_cache(cache, rxcui, default = NULL)

    if (!is.null(atc2_classes)) {
      # Use cached result
    } else {
      # Not in cache - get from API
      atc2_classes <- tryCatch({
        # Add small delay every 20 items to avoid rate limiting
        if (i %% 20 == 0) Sys.sleep(0.2)

        # Call the rxnorm package directly with "second" level
        rxnorm::get_atc(rxcui, "second")
      }, error = function(e) {
        message("\nError for RxCUI '", rxcui, "': ", e$message)
        NA_character_
      })

      # Add to cache
      cache <- adRutils::add_to_cache(cache, rxcui, atc2_classes)
      new_cache_entries <- new_cache_entries + 1

      # Save cache occasionally
      if (new_cache_entries %% 50 == 0) {
        adRutils::save_cache(cache, cache_name, cache_dir)
        message("\nSaved ", new_cache_entries, " new cache entries")
      }
    }

    # Add to mapping data frame
    if (length(atc2_classes) > 0 && !all(is.na(atc2_classes))) {
      for (class in atc2_classes) {
        atc2_mapping <- rbind(atc2_mapping,
                              data.frame(rxcui = rxcui,
                                         atc2_class = class,
                                         stringsAsFactors = FALSE))
      }
    } else {
      # Handle NA case
      atc2_mapping <- rbind(atc2_mapping,
                            data.frame(rxcui = rxcui,
                                       atc2_class = NA_character_,
                                       stringsAsFactors = FALSE))
    }

    # Update progress bar
    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  # Save cache if there are new entries
  if (new_cache_entries > 0 && new_cache_entries %% 50 != 0) {
    adRutils::save_cache(cache, cache_name, cache_dir)
    message("\nSaved ", new_cache_entries, " new cache entries")
  }

  # Add row ID to preserve original order
  dataf <- dplyr::mutate(dataf, .row_id = dplyr::row_number())

  # Rename mapping column to match requested name
  if (new_col_name != "atc2_class") {
    names(atc2_mapping)[names(atc2_mapping) == "atc2_class"] <- new_col_name
  }

  # Join with the mapping table
  result_df <- dataf %>%
    dplyr::left_join(
      atc2_mapping,
      by = setNames("rxcui", rxcui_col),
      relationship = "many-to-many"
    )

  message("Created unnested data frame with ATC2 classifications")
  return(result_df)
}

#' Add ATC2 therapeutic classifications to medications
#'
#' This function takes a data frame with medication information (including RxCUIs)
#' and adds a new column with ATC2 therapeutic classifications (Anatomical
#' Therapeutic Chemical classification system, level 2).
#'
#' @param dataf A data frame containing medication information
#' @param rxcui_col Name of the column containing RxCUI values (default: "rxcui")
#' @param new_col_name Name of the new column to create (default: "atc2_class")
#' @param unnest Logical. If TRUE, returns one row per ATC2 class (default: FALSE)
#' @param cache_name File name for the cache (default: "rxcui_atc2_cache")
#' @param cache_dir Directory name for the cache (default: "cache")
#'
#' @return A data frame with an additional column containing ATC2 classifications.
#'         If unnest=TRUE, medications with multiple classifications will have multiple rows.
#' @export
add_medication_atc2 <- function(dataf,
                                rxcui_col = "rxcui",
                                new_col_name = "atc2_class",
                                unnest = TRUE,
                                cache_name = "rxcui_atc2_cache",
                                cache_dir = "cache") {

  if (unnest) {
    return(add_medication_atc2_unnested(dataf, rxcui_col, new_col_name, cache_name, cache_dir))
  } else {
    return(add_medication_atc2_nested(dataf, rxcui_col, new_col_name, cache_name, cache_dir))
  }
}
