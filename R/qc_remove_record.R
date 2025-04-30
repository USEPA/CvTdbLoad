#' @title qc_remove_record
#' @description Function to remove QC record flagged for removal. Function will account
#' for needed cascade of removal for foreign key table connections 
#' @param df Input dataframe of id, qc_notes, and qc_flags for records to remove
#' @param tbl_name Name of table the records are from
#' @param reset_extraction Boolean whether to reset_extraction or remove whole record. Defaul FALSE.
#' @param del_qc_note Optional note to add to qc_notes and qc_flags field for tbl_name records.
#' @export
#' @return None.
qc_remove_record <- function(df, tbl_name, reset_extraction = FALSE, del_qc_note = NULL){
  
  if(!nrow(df)){
    message("...no records to remove for ", tbl_name, " sheet...")
    return()
  }
  
  if(!is.null(del_qc_note)) del_qc_note = paste0(" - ", del_qc_note)
  
  # Set defaults
  del_ids = list(documents = NULL,
                 studies = NULL,
                 subjects = NULL,
                 series = NULL,
                 conc_time_values = NULL
  )
  
  # Pull associated table foreign key IDs
  switch(tbl_name,
         "Documents" = {
           del_ids$documents = df$id
           del_ids$studies = db_query_cvt(paste0("SELECT distinct id FROM cvt.studies WHERE fk_extraction_document_id IN (", toString(del_ids$documents), ")"))$id
           del_ids$series = db_query_cvt(paste0("SELECT distinct id from cvt.series WHERE fk_study_id in (", toString(del_ids$studies), ")"))$id
           del_ids$subjects = db_query_cvt(paste0("SELECT distinct fk_subject_id as id from cvt.series WHERE fk_study_id in (", toString(del_ids$studies), ")"))$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT distinct id from cvt.conc_time_values WHERE fk_series_id in (", toString(del_ids$series), ")"))$id
         }, "Studies" = {
           del_ids$studies = df$id
           del_ids$series = db_query_cvt(paste0("SELECT distinct id from cvt.series WHERE fk_study_id in (", toString(del_ids$studies), ")"))$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT distinct id from cvt.conc_time_values WHERE fk_series_id in (", toString(del_ids$series), ")"))$id
         }, "Subjects" = {
           del_ids$subjects = df$id
           del_ids$series = db_query_cvt(paste0("SELECT distinct id from cvt.series WHERE fk_subject_id in (", toString(del_ids$subjects), ")"))$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT distinct id from cvt.conc_time_values WHERE fk_series_id in (", toString(del_ids$series), ")"))$id
         }, "Series" = {
           del_ids$series = df$id
           del_ids$conc_time_values = db_query_cvt(paste0("SELECT distinct id from cvt.conc_time_values WHERE fk_series_id in (", toString(del_ids$series), ")"))$id
         }, "Conc_Time_Values" = {
           del_ids$conc_time_values = df$id
         })
  
  # Remove NULL values
  del_ids = del_ids %>%
    purrr::discard(is.null) %>%
    purrr::compact()
  
  print(del_ids)
  
  for(del_tbl in rev(names(del_ids))){
    
    # If not from the provided df table, set generic message for cascade removal qc_notes/flags
    if(del_tbl != tolower(tbl_name)){
      tbl_qc_note = paste0('Removed due to foreign key association to removed record in ', tbl_name, 
                           ' table (', toString(del_ids[[tolower(tbl_name)]]), ")",
                           del_qc_note)
      
      del_df = data.frame(id = del_ids[[del_tbl]]) %>% 
        dplyr::mutate(
          qc_notes = tbl_qc_note,
          qc_flags = tbl_qc_note
        )
    } else {
      tbl_qc_note = paste0("Removed ", tbl_name, 
                           " id ", toString(del_ids[[tolower(tbl_name)]]),
                           del_qc_note)
      
      del_df = df %>%
        dplyr::mutate(
          qc_notes = tbl_qc_note,
          qc_flags = tbl_qc_note
        )
    }
    
    # Skip deleting document records if just resetting extraction
    if(reset_extraction & del_tbl == "documents"){
      next
    }
    
    # Specical case for documents_lineage
    if(del_tbl == "documents"){
      db_query_cvt(paste0("DELETE FROM cvt.documents_lineage WHERE fk_doc_id IN (", toString(del_df$id), ")"))
      db_query_cvt(paste0("DELETE FROM cvt.documents_lineage WHERE fk_parent_doc_id IN (", toString(del_df$id), ")"))
    } else if (del_tbl == "studies"){
      tk_df = db_query_cvt(paste0("SELECT id, fk_study_id FROM cvt.tk_parameters WHERE fk_study_id IN (", toString(del_ids[[tolower(del_tbl)]]), ")"))
      
      if(nrow(tk_df)){
        tbl_qc_note = paste0('Removed due to foreign key association to removed record in ', del_tbl, 
                             ' table (', fk_study_id, ")",
                             del_qc_note)
        tk_df = tk_df %>%
          dplyr::mutate(
            qc_notes = tbl_qc_note,
            qc_flags = tbl_qc_note
          )
        # Update database entry twice:
        # once to audit old record
        db_update_tbl(df = tk_df, tblName = "tk_parameters")
        # twice to audit QC'd record
        db_update_tbl(df = tk_df, tblName = "tk_parameters")
        # Delete database entries
        db_query_cvt(paste0("DELETE FROM cvt.tk_parameters WHERE id IN (", toString(tk_df$id), ")"))
        # tk_parameters
        # tk_parameters_studies     
      }
    } else if (del_tbl == "series"){
      tk_df = db_query_cvt(paste0("SELECT id, fk_series_id FROM cvt.tk_parameters WHERE fk_series_id IN (", toString(del_ids[[tolower(del_tbl)]]), ")"))
      
      if(nrow(tk_df)){
        tbl_qc_note = paste0('Removed due to foreign key association to removed record in ', del_tbl, 
                             ' table (', fk_series_id, ")",
                             del_qc_note)
        tk_df = tk_df %>%
          dplyr::mutate(
            qc_notes = tbl_qc_note,
            qc_flags = tbl_qc_note
            )
        # Update database entry twice:
        # once to audit old record
        db_update_tbl(df = tk_df, tblName = "tk_parameters")
        # twice to audit QC'd record
        db_update_tbl(df = tk_df, tblName = "tk_parameters")
        # Delete database entries
        db_query_cvt(paste0("DELETE FROM cvt.tk_parameters WHERE id IN (", toString(tk_df$id), ")"))
        # tk_parameters
        # tk_parameters_series  
      }
    }
    
    message("...removing records for ", del_tbl, " table based on sheet ", tbl_name)
    # Update database entry twice:
    # once to audit old record
    db_update_tbl(df = del_df, tblName = del_tbl)
    # twice to audit QC'd record
    db_update_tbl(df = del_df, tblName = del_tbl)
    # Check if audit worked
    curr_rec = db_query_cvt(paste0("SELECT id, version FROM cvt.", del_tbl, 
                                   " WHERE id in (", toString(del_df$id), ")"))
    audit_check = db_query_cvt(paste0("SELECT * FROM cvt.cvt_audit WHERE fk_table_id in (", 
                                      toString(del_df$id), ") AND fk_table_name = '", del_tbl,"' AND ",
                                      "version in (", toString(unique(curr_rec$version) - 1), ")"))
    # If audit successful, delete the record
    if(nrow(audit_check)){
      # Delete database entries
      db_query_cvt(paste0("DELETE FROM cvt.", del_tbl, " WHERE id IN (", toString(del_df$id), ")"))  
    } else {
      stop("Record not audited as expected...record not deleted...")
    }
  }
}