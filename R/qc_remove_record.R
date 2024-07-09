#' @title qc_remove_record
#' @description Function to remove QC record flagged for removal. Function will account
#' for needed cascade of removal for foreign key table connections 
#' @param df Input dataframe of id, qc_notes, and qc_flags for records to remove
#' @param tbl_name Name of table the records are from
#' @export
#' @return None.
qc_remove_record <- function(df, tbl_name){
  
  # Set defaults
  del_ids = list(documents = NULL,
                 documents_lineage = NULL,
                 studies = NULL,
                 subjects = NULL,
                 series = NULL,
                 conc_time_values = NULL
  )
  
  # Pull associated table foreign key IDs
  switch(tbl_name,
         "Documents" = {
           del_ids$documents = df$id
           del_ids$studies = db_query_cvt(paste0("SELECT id FROM cvt.studies WHERE fk_extraction_document_id IN (", toString(docs_rm), ")"))$id
           del_ids$documents_lineage = db_query_cvt(paste0("SELECT fk_doc_id, fk_parent_doc_id FROM cvt.documents_lineage WHERE fk_doc_id IN (", toString(docs_rm), ") ",
                                                           "OR fk_parent_doc_id IN (", toString(docs_rm), ")"))$id
           del_ids$series = db_query_cvt(paste0("SELECT id from cvt.series WHERE fk_study_id in (", toString(studies_rm), ")"))$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT id from cvt.conc_time_values WHERE fk_series_id in (", toString(series_rm), ")"))$id
         }, "Studies" = {
           del_ids$studies = df$id
           del_ids$series = db_query_cvt(paste0("SELECT id from cvt.series WHERE fk_study_id in (", toString(studies_rm), ")"))$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT id from cvt.conc_time_values WHERE fk_series_id in (", toString(series_rm), ")"))$id
         }, "Subjects" = {
           del_ids$subjects = df$id
           del_ids$series = db_query_cvt(paste0("SELECT id from cvt.series WHERE fk_subject_id in (", toString(subjects_rm), ")"))$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT id from cvt.conc_time_values WHERE fk_series_id in (", toString(series_rm), ")"))$id
         }, "Series" = {
           del_ids$series = df$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT id from cvt.conc_time_values WHERE fk_series_id in (", toString(series_rm), ")"))$id
         }, "Conc_Time_Values" = {
           del_ids$conc_time_values = df$id
         })
  
  # Remove NULL values
  del_ids = del_ids %>%
    purrr::compact()
  
  for(del_tbl in names(del_ids)){
    
    # If not from the provided df table, set generic message for cascade removal qc_notes/flags
    if(del_tbl != tolower(tbl_name)){
      df = data.frame(id = del_ids[[del_tbl]]) %>% 
        dplyr::mutate(qc_notes = paste0('Removed due to foreign key association to removed record in ', tbl_name, ' table'),
                      qc_flags = paste0('Removed due to foreign key association to removed record in ', tbl_name, ' table'))
    }
    
    # Specical case for documents_lineage
    if(del_tbl == "documents_lineage"){
      db_query_cvt(paste0("DELETE FROM cvt.", del_tbl, " WHERE fk_doc_id IN (", toString(df$id), ")"))
      db_query_cvt(paste0("DELETE FROM cvt.", del_tbl, " WHERE fk_parent_doc_id IN (", toString(df$id), ")"))
    }
    
    # Update database entry twice:
    # once to audit old record
    # db_update_tbl(df = df, tblName = del_tbl)
    # twice to audit QC'd record
    # db_update_tbl(df = df, tblName = del_tbl)
    # Delete database entries
    # db_query_cvt(paste0("DELETE FROM cvt.", del_tbl, " WHERE id IN (", toString(df$id), ")"))
  }
}