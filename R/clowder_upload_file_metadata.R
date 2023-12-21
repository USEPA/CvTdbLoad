upload_file_metadata <- function(metadata, dsID, userID, baseurl, apiKey){
  ################################################################################
  ### Push metadata
  ################################################################################
  
  c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey) %>%
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