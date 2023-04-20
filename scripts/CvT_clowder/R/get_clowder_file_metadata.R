get_clowder_file_metadata <- function(fileID = NULL, apiKey = NULL){
  Sys.sleep(0.25)
  md = GET(paste0("https://clowder.edap-cluster.com/api/files/",
                  fileID,
                  "/metadata.jsonld",
                  apiKey)) %>%
    content()
 # return(lapply(md[[1]]$content, function(x){ ifelse(is_null(x), NA, x) }) %>%
 #          rbind.data.frame())
  if(is_empty(md)){
    return(md)
  } else {
    return(lapply(md[[1]]$content, function(x){ ifelse(is_null(x), 
                                                       as.character(NA), x) }))
  }
}
