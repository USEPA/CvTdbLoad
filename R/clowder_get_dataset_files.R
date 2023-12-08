clowder_get_dataset_files <- function(dsID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)
  # Pull all Clowder Files from input dataset
  c_files_list = httr::GET(
    paste0(baseurl, "/api/datasets/", dsID,"/listAllFiles?limit=0"),
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()
  # Format data
  c_files_list = lapply(c_files_list, function(f){
    f %>%
      data.frame() %>%
      tidyr::unnest(cols=c())
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(clowder_id = id, `folders.name`, filename) %>%
    return()
}