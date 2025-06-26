clowder_get_file_metadata <- function(fileID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)
  
  # Format URL for request
  url = paste0(baseurl, "/api/files/metadata.jsonld?id=", 
               # Combine multiple file ID values if provided
               paste0(fileID, collapse="&id="), 
               "&?limit=0")
  # Pull metadata for input files
  metadata = httr::GET(
    url=url,
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()
  
  # Format data to return (combine across multiple metadata submissions)
  lapply(metadata, function(f){
    lapply(f, function(ff){
      tmp = ff %>%
        purrr::pluck("content") %>%
        purrr::compact() %>%
        data.frame() %>%
        # TODO Suppress new name print output
        tidyr::unnest(cols=c()) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.)))
    }) %>%
      dplyr::bind_cols()
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(clowder_id = names(metadata)) %>%
    return()
}
