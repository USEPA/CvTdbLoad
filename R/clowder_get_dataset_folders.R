#' @title FUNCTION_TITLE
#' @description Pull a dataframe of folder metadata stored in a Clowder dataset.
#' @param dsID Clowder dataset identifier.
#' @param baseurl Clowder base URL.
#' @param apiKey Clowder API key.
#' @return Dataframe with Clowder folder identifier and folder name.
#' @seealso 
#'  \code{\link[httr]{GET}}, \code{\link[httr]{content_type}}, \code{\link[httr]{add_headers}}, \code{\link[httr]{content}}
#'  \code{\link[tidyr]{unnest}}
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{select}}
#' @rdname clowder_get_dataset_folders
#' @export 
#' @importFrom httr GET accept_json content_type_json add_headers content
#' @importFrom tidyr unnest
#' @importFrom dplyr bind_rows select
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
