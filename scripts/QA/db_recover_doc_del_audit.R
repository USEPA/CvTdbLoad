#' @title db_recover_doc_del_audit
#' @description Function to recover deleted document associated records from audit table
#' @param doc_id ID of document from Documents table to recover.
#' @param schema PostgreSQL database schema.
#' @param revert_document Whether to revert current document record with audit entry. Default FALSE.
#' @return None. Database SQL queries are performed.
db_recover_doc_del_audit <- function(doc_id, schema, revert_document = FALSE){
  message("Pulling document and audit information...")
  # Check if doc_id present in documents table
  doc_check = db_query_cvt(paste0("SELECT * FROM ", schema, ".documents ",
                                  "WHERE id = ", doc_id))
  
  # Check if doc_id present in cvt_audit table
  audit_check = db_query_cvt(paste0("SELECT * FROM ", schema, ".cvt_audit ",
                                    "WHERE fk_table_id = ", doc_id, " and ",
                                    "fk_table_name = 'documents'"))
  
  if(!nrow(audit_check) & !nrow(doc_check)){
    message("Document ID '", doc_id, "' not found in documents or cvt_audit table...returning...")
    return()
  }
  
  audit_sheets = list()
  
  audit_sheets$Documents = audit_check
  
  # Pull studies table associations
  audit_sheets$Studies = db_query_cvt(paste0('SELECT * ',
                                             'FROM ', schema, '.cvt_audit WHERE record like \'%"fk_extraction_document_id" : "',
                                             doc_id,'"%\''))
  
  if(nrow(audit_sheets$Studies)){
    # Create chain of "like" statements by study_id
    like_chain = paste0('record like \'%"fk_study_id" : "', 
                        unique(audit_sheets$Studies$fk_table_id),'"%\'', 
                        collapse = " OR ")
    
    # Pull series table associations by fk_study_id
    audit_sheets$Series = db_query_cvt(paste0("SELECT * ",
                                              "FROM ", schema, ".cvt_audit WHERE (", like_chain, ") and ",
                                              "fk_table_name = 'series'"))
  }
  
  if(nrow(audit_sheets$Series)){
    # Create chain of "like" statements by study_id
    like_chain = paste0('record like \'%"fk_series_id" : "', 
                        unique(audit_sheets$Series$fk_table_id),'"%\'', 
                        collapse = " OR ")
    
    # Pull conc_time_values table associations by fk_series_id
    audit_sheets$Conc_Time_Values = db_query_cvt(paste0("SELECT * ",
                                                        "FROM ", schema, ".cvt_audit WHERE (", like_chain, ") and ",
                                                        "fk_table_name = 'conc_time_values'"))
    
    # Pull tk_parameters table associations by fk_series_id
    audit_sheets$tk_parameters = db_query_cvt(paste0("SELECT * ",
                                                     "FROM ", schema, ".cvt_audit WHERE (", like_chain, ") and ",
                                                     "fk_table_name = 'tk_parameters'"))
  }
  
  # Filter to only those with records
  audit_sheets = purrr::keep(audit_sheets, ~nrow(.) > 0)
  message("...expanding stored audit records")
  audit_sheets = lapply(names(audit_sheets), function(sheet){
    # Expand record field
    record_list = lapply(seq_len(nrow(audit_sheets[[sheet]])), function(i){
      audit_sheets[[sheet]]$record[i] %>%
        # Paste into an array so NULL values are converted to NA
        paste0('[', ., ']') %>%
        # Convert JSON into dataframe
        jsonlite::fromJSON() %>%
        as.data.frame() %>%
        # Add key for join
        dplyr::mutate(fk_table_id = audit_sheets[[sheet]]$fk_table_id[i],
                      version = audit_sheets[[sheet]]$version[i])
    }) %>%
      dplyr::bind_rows()
    
    # Return expanded record dataframe
    audit_sheets[[sheet]] %>%
      dplyr::left_join(record_list,
                       by=c("fk_table_id", "version")) %>%
      dplyr::mutate(qc_status = NA) %>%
      return()
    
  }) %T>% {
    names(.) <- names(audit_sheets)
  }
  
  if(nrow(audit_sheets$Series)){
    
    # Pull subjects table associations by fk_subject_id
    audit_sheets$Subjects = db_query_cvt(paste0("SELECT * FROM ", schema, ".cvt_audit ",
                                                "WHERE fk_table_id in (", toString(audit_sheets$Series$fk_subject_id), 
                                                ") and ",
                                                "fk_table_name = 'subjects'"))
    
    # Expand record field
    record_list = lapply(seq_len(nrow(audit_sheets$Subjects)), function(i){
      audit_sheets$Subjects$record[i] %>%
        # Paste into an array so NULL values are converted to NA
        paste0('[', ., ']') %>%
        # Convert JSON into dataframe
        jsonlite::fromJSON() %>%
        as.data.frame() %>%
        # Add key for join
        dplyr::mutate(fk_table_id = audit_sheets$Subjects$fk_table_id[i],
                      version = audit_sheets$Subjects$version[i])
    }) %>%
      dplyr::bind_rows()
    
    # Join expanded record dataframe
    audit_sheets$Subjects = audit_sheets$Subjects %>%
      dplyr::left_join(record_list,
                       by=c("fk_table_id", "version")) %>%
      dplyr::mutate(qc_status = NA)
  }
  
  if(revert_document){
    # Delete document record and revert back to audit
    # db_query_cvt(paste0("DELETE FROM ", schema, ".documents WHERE id = ", doc_id))
  } else {
    # If not revert document, remove Documents sheet from audit list
    audit_sheets$Documents = NULL
  }
  
  # Pull table field information
  tbl_field_list = db_query_cvt(paste0("SELECT table_name, column_name FROM information_schema.columns WHERE table_schema='", schema,"'"))
  
  # Filter to only those with records
  audit_sheets = purrr::keep(audit_sheets, ~nrow(.) > 0)
  
  # Orchestrate "Add" back for audited records
  for(sheet in c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values", "tk_parameters")){
    # Skip if sheet not present
    if(!sheet %in% names(audit_sheets)) next
    # Get allowed field names
    tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == tolower(sheet)]
    message("...pushing records for ", sheet, " sheet")
    # Select data and fields to push
    df = audit_sheets[[sheet]] %>%
      dplyr::group_by(fk_table_id) %>%
      # Remove max value since we want the version before the delete qc_note
      dplyr::slice_min(version, n=-1) %>%
      # Select max
      dplyr::slice_max(version) %>%
      dplyr::rename(audit_id = id, id = fk_table_id) %>%
      dplyr::select(dplyr::any_of(tbl_fields))
    # Push records
    db_push_rs = db_push_tbl_to_db(dat=df,
                                   tblName=tolower(sheet),
                                   overwrite=FALSE, 
                                   append=TRUE)
    # Check if push was successful
    if(is.null(db_push_rs) || is.na(db_push_rs)){
      message("Issue pushing ",sheet," table.")
      browser()
    } else {
      # Filter to audit records to delete since they're being reverted
      del_df = audit_sheets[[sheet]] %>%
        dplyr::left_join(df %>%
                           dplyr::select(fk_table_id = id, del_version = version)) %>%
        dplyr::group_by(fk_table_id) %>%
        dplyr::filter(version >= del_version) %>%
        dplyr::ungroup() %>%
        dplyr::select(id, fk_table_name, fk_table_id, version, del_version)
      # If successfully added back, delete added audit records
      db_query_cvt(paste0("DELETE FROM ", schema, ".cvt_audit WHERE id in (", toString(del_df$id), ")"))
    }
  }
  message("Done.")
}