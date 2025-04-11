#' @description This is a helper function to match already loaded CvT document entries to Clowder docs.
#' @param dsID Clowder dataset ID to pull from.
#' @param apiKey The API key required for a user to access the Clowder dataset
#' @return Returns a dataframe with file details of: filename and ClowderID.
#' @import dplyr
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [select][dplyr::select]
#'  [dbWriteTable][RPostgres::dbWriteTable], [dbDisconnect][RPostgres::dbDisconnect]
#' @rdname clowder_match_post_upload
#' @export 
#' @importFrom dplyr filter select
#' @importFrom RPostgres dbWriteTable dbDisconnect
clowder_match_post_upload <- function(dsID = NULL, apiKey = NULL){
  #Get all documents in CvT without Clowder ID
  docs = db_query_cvt("SELECT id, pmid, other_study_identifier FROM cvt.documents where clowder_file_id is NULL") %>%
    dplyr::filter(!is.na(pmid) | !is.na(other_study_identifier))
  #Match to Clowder ID
  output = match_clowder_docs(df=docs, dsID=dsID, apiKey=apiKey) %>%
    dplyr::filter(!is.na(clowder_file_id)) %>%
    dplyr::select(id, clowder_file_id)
  #Push to CvT
  #Push updates
  con = db_connect_to_CvT()
  RPostgres::dbWriteTable(con, value = output, name=c("cvt", "temp_tbl"), overwrite=TRUE, row.names=FALSE)  
  RPostgres::dbDisconnect(con)
  
  query = paste0("UPDATE cvt.documents h SET clowder_file_id = m.clowder_file_id",
                 " FROM cvt.temp_tbl m",
                 " WHERE h.id = m.id")  
  #Make update (only uncomment when ready to use)
  #db_query_cvt(query=query)
  db_query_cvt("DROP TABLE cvt.temp_tbl")
}
