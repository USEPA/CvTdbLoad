#' @title clowder_upload_file_metadata
#' @description Function to process add metadata field values to Clowder files.
#' @param metadata Dataframe of metadata to add to Clowder files.
#' @param dsID Clowder dataset identifier.
#' @param userID Clowder User identifier.
#' @param baseurl Clowder base URL.
#' @param apiKey Clowder API key.
#' @return None. Clowder APi calls are performed to add metadata to Clowder files.
#' @seealso 
#'  \code{\link[tidyr]{unite}}, \code{\link[tidyr]{separate}}
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{filter}}
#'  \code{\link[purrr]{flatten}}
#'  \code{\link[httr]{POST}}, \code{\link[httr]{content_type}}, \code{\link[httr]{add_headers}}
#' @rdname clowder_upload_file_metadata
#' @export 
#' @importFrom tidyr unite separate
#' @importFrom dplyr left_join rename select filter
#' @importFrom purrr flatten
#' @importFrom httr POST accept_json content_type_json add_headers
clowder_upload_file_metadata <- function(metadata, dsID, userID, baseurl, apiKey){
  ################################################################################
  ### Push metadata
  ################################################################################
  
  c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey)
  
  # No Clowder files to update
  if(!nrow(c_files_list)){
    return()
  }
    
  c_files_list = c_files_list %>%
    tidyr::unite(join_key, `folders.name`, filename, sep="_cvtjoin_")
  
  # Map metadata to Clowder Doc
  metadata = metadata %>%
    tidyr::unite(join_key, `Issue key`, filename, sep="_cvtjoin_") %>%
    dplyr::left_join(c_files_list,
                     by="join_key") %>%
    tidyr::separate(join_key, into = c("jira_ticket_id", "filename"), sep="_cvtjoin_") %>%
    dplyr::rename(jira_upload_date=date) %>%
    dplyr::select(-attachment_name, -filename) %>%
    dplyr::filter(!is.na(clowder_id))
  
  # Prep metadata JSON with userID
  md = list(
    "@context"= c(
      "https://clowder.ncsa.illinois.edu/contexts/metadata.jsonld",
      list("@vocab"= "https://clowder.ncsa.illinois.edu/contexts/metadata.jsonld")
    ),
    "agent"= list(
      "@type"= "cat:user",
      "user_id"= paste0("http://clowder-test.edap-cluster.com/api/users/", userID)
    ),
    "content"= c()
  )
  
  # add all metadata
  cat("...pushing metadata to files...\n")
  
  for(u in unique(metadata$clowder_id)){
    cat("...pushing Clowder File ", u, " (", which(metadata$clowder_id == u), " of ", length(unique(metadata$clowder_id)),")\n")
    content = metadata[metadata$clowder_id == u,]
    
    # Compare metadata
    old_metadata = clowder_get_file_metadata(fileID = u, baseurl, apiKey)
    
    compare_metadata = old_metadata %>%
      dplyr::select(any_of(names(content)))
    
    # Skip if old metadata matches new metadata
    if(identical(content, compare_metadata)) next
    
    # Create dictionary of metadata name value pairs
    md$content = metadata %>%
      dplyr::filter(clowder_id == u) %>%
      dplyr::select(-clowder_id) %>%
      purrr::flatten()
    # POST metadata for 'u' file
    # Rest between requests
    Sys.sleep(0.25)
    httr::POST(
      paste0(baseurl, "/api/files/",u ,"/metadata.jsonld"),
      httr::accept_json(),
      httr::content_type_json(),
      # Use API Key for authorization
      httr::add_headers(`X-API-Key` = apiKey),
      encode = "json",
      body=md
    )
  }
  cat("Done...\n")
}
