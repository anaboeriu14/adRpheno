#' Get single RxCUI from RxNorm API with retry logic
#'
#' Makes a single API call to RxNorm to retrieve the RxCUI for one medication.
#' This function is exported so users can get individual RxCUIs when needed.
#'
#' @param med_name Character. Single medication name to look up
#' @param method Character. Either "exact" or "approximate" matching
#' @param retry_count Integer. Number of retry attempts on failure (default: 3)
#'
#' @return Character. RxCUI if found, NA_character_ if not found or error
#' @export
#'
#' @examples
#' \dontrun{
#' # Get single RxCUI
#' rxcui <- get_single_rxcui("aspirin", method = "exact")
#' print(rxcui)  # "1191"
#'
#' # With approximate matching
#' rxcui <- get_single_rxcui("aspirn", method = "approximate")
#' }
get_single_rxcui <- function(med_name, method = "exact", retry_count = 3) {

  # Simple validation for single function
  if (!is.character(med_name) || length(med_name) != 1) {
    stop("med_name must be a single character string")
  }
  if (!method %in% c("exact", "approximate")) {
    stop("method must be 'exact' or 'approximate'")
  }
  if (is.na(med_name) || med_name == "") {
    return(NA_character_)
  }

  # Try multiple attempts
  for (attempt in seq_len(retry_count)) {
    rxcui <- tryCatch({
      # Build URL based on method
      if (method == "exact") {
        url <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?name=",
                      utils::URLencode(med_name), "&search=2")
      } else {
        url <- paste0("https://rxnav.nlm.nih.gov/REST/approximateTerm.json?term=",
                      utils::URLencode(med_name))
      }

      # Make API call
      response <- httr::GET(url, httr::timeout(10))

      if (httr::status_code(response) == 200) {
        json_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

        # Extract RxCUI based on method
        if (method == "exact") {
          .null_coalesce(json_data$idGroup$rxnorm[1], NA_character_)
        } else {
          .null_coalesce(json_data$approximateGroup$candidate$rxcui[1], NA_character_)
        }
      } else {
        NA_character_
      }
    }, error = function(e) {
      if (attempt == retry_count) {
        message("Error processing '", med_name, "': ", e$message)
      }
      NA_character_
    })

    # If we got a valid result, return it
    if (!is.na(rxcui)) {
      return(rxcui)
    }

    # Wait before retry (except on last attempt)
    if (attempt < retry_count) {
      Sys.sleep(1)
    }
  }

  return(NA_character_)
}


#' Get RxCUIs for medications using exact or approximate matching
#'
#' This function takes a data frame with medication names and adds a new column
#' with corresponding RxCUI identifiers from the RxNorm database.
#'
#' @param dataf Data frame containing medication names
#' @param med_column Character. Column name containing medication names
#' @param rxcui_column Character. Name for the new RxCUI column (default: "rxcui")
#' @param method Character. "exact" or "approximate" matching (default: "exact")
#' @param batch_size Integer. Number of medications per batch (default: 200)
#' @param save_freq Integer. Save cache every N items (default: 50)
#' @param cache_dir Character. Cache directory (default: "cache")
#' @param max_age_days Integer. Cache expiration in days (default: 30)
#' @param retry_count Integer. Number of retry attempts (default: 3)
#' @param batch_delay Numeric. Seconds between batches (default: 2)
#'
#' @return Data frame with RxCUI column added
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- data.frame(medication = c("aspirin", "tylenol", "ibuprofen"))
#' df_with_rxcuis <- add_rxcuis(df, med_column = "medication")
#'
#' # With approximate matching
#' df_with_rxcuis <- add_rxcuis(df, med_column = "medication", method = "approximate")
#' }
add_rxcuis <- function(dataf,
                       med_column,
                       rxcui_column = "rxcui",
                       method = "exact",
                       batch_size = 200,
                       save_freq = 50,
                       cache_dir = "cache",
                       max_age_days = 30,
                       retry_count = 3,
                       batch_delay = 2) {

  start_time <- Sys.time()

  # === STEP 1: Validate all inputs ===
  .validate_rxcui_inputs(dataf, med_column, rxcui_column, method,
                         batch_size, save_freq, cache_dir, max_age_days,
                         retry_count, batch_delay)

  # === STEP 2: Setup result data frame ===
  result_df <- .setup_result_dataframe(dataf, rxcui_column)

  # === STEP 3: Get unique medications to process ===
  medication_info <- .get_unique_medications(dataf, med_column)

  if (medication_info$count == 0) {
    message("No valid medication names found")
    return(result_df)
  }

  # === STEP 4: Process medications using unified batch processor ===
  message("Processing ", medication_info$count, " unique medications...")

  cache_name <- paste0("rxcui_", method, "_cache")

  rxcui_results <- .process_batch(
    items = medication_info$unique_meds,
    api_function = get_single_rxcui,
    batch_size = batch_size,
    save_freq = save_freq,
    cache_name = cache_name,
    cache_dir = cache_dir,
    max_age_days = max_age_days,
    retry_count = retry_count,
    batch_delay = batch_delay,
    process_type = "medications",
    method = method
  )

  # === STEP 5: Apply results back to original data frame ===
  result_df[[rxcui_column]] <- rxcui_results[result_df[[med_column]]]

  # === STEP 6: Report results and return ===
  .report_results(result_df, dataf, rxcui_column, med_column, start_time, method)

  return(result_df)
}
