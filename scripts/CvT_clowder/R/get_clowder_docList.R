#'@title Get Clowder Document List
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param clowderDir The file directory/dataset name on Clowder.
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@return Returns a dataframe with file details of: filename and ClowderID.
#'@import dplyr
clowder_get_docList <- function(clowderDir=NULL, apiKey=NULL){
  baseurl = "https://clowder.edap-cluster.com/api"
  #Get dataset IDs of interest
  dataset_info <- httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets",
                                   apiKey)) %>% httr::content() %>%
    tibble(id = map_chr(.,"id"), name = map_chr(.,"name")) %>%
    select(-1) %>%
    filter(name %in% clowderDir)
  
  #get list of all files already uploaded.
  return(lapply(dataset_info$id, function(x){
    Sys.sleep(0.25)
    tmp = paste0(baseurl,"/datasets/",x,"/listAllFiles",apiKey)
    return(res <- GET(tmp) %>% 
             content() %>%
             tibble(filename = map_chr(.,"filename"), 
                    id = map_chr(.,"id")) %>% 
             select(-1)
    )
  }) %>% jsonlite::rbind_pages())
}
