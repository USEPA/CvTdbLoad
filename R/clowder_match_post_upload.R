#' @title clowder_match_post_upload
#' @description This is a helper function to match already loaded CvT document entries to Clowder docs.
#' @param dsID Clowder dataset identifier.
#' @param baseurl Clowder base URL.
#' @param apiKey Clowder API key.
#' @return None. An update query is performed to update the Documents table "clowder_file_id" field.
#' @seealso 
#'  [filter][dplyr::filter], [select][dplyr::select]
#' @rdname clowder_match_post_upload
#' @export
#' @importFrom dplyr filter select
#' @importFrom DBI dbWriteTable dbDisconnect
clowder_match_post_upload <- function(dsID = NULL, baseurl = NULL, apiKey = NULL){
  # Get all documents in CvT without Clowder ID
  docs = db_query_cvt("SELECT id, pmid, other_study_identifier FROM cvt.documents where clowder_file_id is NULL") %>%
    dplyr::filter(!is.na(pmid) | !is.na(other_study_identifier))
  # Match to Clowder ID
  output = clowder_match_docs(df=docs,
                              dsID=doc_dsID,
                              baseurl=baseurl,
                              apiKey=apiKey) %>%
    dplyr::filter(!is.na(clowder_file_id)) %>%
    dplyr::select(id, clowder_file_id)
  # Push updates to documents table
  db_update_tbl(df = output,
                tblName = "documents")
}
