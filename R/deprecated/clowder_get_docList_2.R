#' @title Get Clowder Document List (varient of the other function in utilities)
#' @description This is a helper function to get a list of documents available in a Clowder dataset
#' @param dsID Clowder dataset ID to pull from.
#' @param apiKey The API key required for a user to access the Clowder dataset
#' @return Returns a dataframe with file details of: filename and ClowderID.
#' @import dplyr
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [GET][httr::GET], [content][httr::content]
#'  [tibble][tidyr::tibble]
#'  [map_chr][purrr::map_chr]
#'  [select][dplyr::select]
#' @rdname clowder_get_docList_2
#' @export 
#' @importFrom httr GET content
#' @importFrom tidyr tibble
#' @importFrom purrr map_chr
#' @importFrom dplyr select
clowder_get_docList_2 <- function(dsID=NULL, apiKey=NULL){
  Sys.sleep(0.25) #Wait between requetss
  baseurl = "https://clowder.edap-cluster.com/api"
  #Get dataset IDs of interest
  httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets/",
                                   dsID,"/files?key=",
                                   apiKey)) %>% httr::content() %>%
    tidyr::tibble(clowder_file_id = purrr::map_chr(.,"id"), filename = purrr::map_chr(.,"filename")) %>%
    dplyr::select(-1) %>%
    return()
}
