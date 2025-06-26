#' @title get_mw_chemicals_api
#' @description Function to get molecular weight dictionary from CCTE Chemicals API in batches.
#' @param api_key API key for the CCTE Chemicals API.
#' @param dtxsid_list List of DTXSIDs to query.
#' @return Dataframe of molecular weight information by DTXSID.
#' @seealso 
#'  \code{\link[httr]{GET}}, \code{\link[httr]{content}}, \code{\link[httr]{POST}}, \code{\link[httr]{content_type}}, \code{\link[httr]{add_headers}}
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{filter}}
#' @rdname get_mw_chemicals_api
#' @export 
#' @importFrom httr GET content POST accept_json content_type_json add_headers
#' @importFrom dplyr bind_rows select filter
get_mw_chemicals_api <- function(dtxsid_list, api_key){

  # Test of API is up and running
  api_test <- httr::GET("https://api-ccte.epa.gov/docs/chemical.html") %>%
    httr::content()
  
  # Use bulk DTXSID CCTE Chemicals API pull (limit 200 per call)
  if(!is.null(api_key) & !grepl("404 Not Found", api_test)){
    cat("...Pulling DSSTox mw using CCTE API...\n")
    # Split list into subsets of 200
    mw <- dtxsid_list %>%
      # Filter out missingness
      .[!dtxsid_list %in% c(NA, "NA", "-", "")] %>%
      split(., rep(1:ceiling(length(.)/200), each=200, length.out=length(.)))
    # Loop through the groups of 200 DTXSID values
    for(i in seq_along(mw)){
      # Wait between calls for API courtesy
      Sys.sleep(0.25)
      cat("...Pulling DSSTox mw ", i , " of ", length(mw), "\n")
      mw[[i]] <- httr::POST(
        "https://api-ccte.epa.gov/chemical/detail/search/by-dtxsid/",
        httr::accept_json(),
        httr::content_type_json(),
        # Use API Key for authorization
        httr::add_headers(`x-api-key` = api_key),
        encode = "json",
        body=as.list(mw[[i]])
      ) %>%
        httr::content() %>%
        dplyr::bind_rows()
      # Edge case of not having averageMass returned
      if(!"averageMass" %in% names(mw[[i]])) mw[[i]]$averageMass = NA
      # Select mw field and filter out NA values
      mw[[i]] = mw[[i]] %>%
        dplyr::select(dtxsid, mw=averageMass) %>%
        dplyr::filter(!is.na(mw))
    }
    # Combine all results
    mw = dplyr::bind_rows(mw)
  }
  
  return(mw)
}
