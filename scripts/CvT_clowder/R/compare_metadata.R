compare_metadata <- function(fileID=NULL, new_metadata=NULL, apiKey=NULL){
  return(!identical(get_clowder_file_metadata(fileID=fileID, apiKey=apiKey), 
                    new_metadata))
}
