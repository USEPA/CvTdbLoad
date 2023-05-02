#'@title Download Clowder Documents by File ID and Filename
#'@description This is a helper function to download documents using the Clowder API. It also checks if a fileID/fileName has already been downloaded and skips if it exists
#'@param docData Dataframe containing fileID and fileName columns for the Clowder API download
#'@param fileName A character list of file names (with file extension) to name the downloaded files
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@param limit A numeric integer value to specify the number of documents to pull from the list of file IDs (Leave NULL if you want all to be downloaded)
#'@return None. Files are downloaded into specified output directory
#'@import downloader
#'@export
clowder_download_docs <- function(docData=NULL, outputDir, apiKey, limit=NULL){
  if(is.null(docData)) stop("Error: Must provide docData to download clowder docs")
  #Filter list by what is already downloaded in outputDir
  tmp <- list.files(outputDir)
  #Remove fileId and fileNames from already downloaded docs
  fileName <- docData$filename[!(docData$filename %in% tmp)] #Filter out fileNames where fileNames have already been downloaded
  fileID <- docData$id[which(docData$filename %in% fileName)] #Filter out fileIDs where fileNames have already been downloaded  
  
  if(is.null(limit)){ limit <- length(fileID) } #No limit input, pull all documents
  message("Downloading ", limit, " new docs from Clowder API...")
  if(limit){
    invisible(lapply(seq_len(limit), function(x){
      tryCatch({
        downloader::download(paste0("https://clowder.edap-cluster.com/api/files/", 
                                    fileID[x], apiKey), 
                             paste0(outputDir, 
                                    #sub("^.*?([A-Z])", "\\1", 
                                    fileName[x]
                                    #   )
                             ), #Remove starting hashkey string before first capitalization
                             mode = "wb", 
                             quiet=TRUE)
      },
      error=function(cond){ message("Error message: ", cond); return(NA) },
      warning=function(cond){ message("Warning message: ", cond); return(NULL) }
      )
    }))
  }
}
