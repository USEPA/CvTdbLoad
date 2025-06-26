clowder_get_dataset_folders <- function(dsID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)
  # Pull all Clowder folders from input dataset
  c_folders_list = httr::GET(
    paste0(baseurl, "/api/datasets/", dsID,"/folders?limit=0"),
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()
  # Format data
  c_folders_list = lapply(c_folders_list, function(f){
    f %>%
      data.frame() %>%
      tidyr::unnest(cols=c())
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(folder_id = id, foldername = name) %>%
    return()
}
