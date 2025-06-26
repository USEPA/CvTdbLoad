#' @title clowder_get_dataset_files
#' @description Pull a dataframe of file metadata stored in a Clowder dataset.
#' @param dsID Clowder dataset identifier.
#' @param baseurl Clowder base URL.
#' @param apiKey Clowder API key.
#' @return Dataframe with Clowder file identifier, folder name, and file name.
#' @seealso 
#'  \code{\link[httr]{GET}}, \code{\link[httr]{content_type}}, \code{\link[httr]{add_headers}}, \code{\link[httr]{content}}
#'  \code{\link[tidyr]{unnest}}
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{select}}
#' @rdname clowder_get_dataset_files
#' @export 
#' @importFrom httr GET accept_json content_type_json add_headers content
#' @importFrom tidyr unnest
#' @importFrom dplyr bind_rows select
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
  
  # No files found, return empty dataframe
  if(!length(c_files_list)){
    message("No Clowder Dataset files found...returning...")
    return(data.frame())
  }
  
  # Format returned file data
  c_files_list = lapply(c_files_list, function(f){
    f %>%
      data.frame() %>%
      tidyr::unnest(cols=c())
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(clowder_id = id, `folders.name`, filename) %>%
    return()
}
