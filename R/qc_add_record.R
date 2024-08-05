#' @title qc_add_record
#' @description Function to add record from QC template 
#' @param df Input dataframe of field values to add to database table
#' @param tbl_name Name of table the records are from
#' @export
#' @return Modified version of input df with mapped new database ID values
qc_add_record <- function(df, tbl_field_list, load_doc_sheet_only){
  
  # TODO Finish adapting from push_CvT_templates_orig_values.R
  
  message("...pushing to Documents table")
  # Get documents table fields
  tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == "documents"] %>%
    .[!. %in% col_exclude]
  browser()
  db_push_rs = db_push_tbl_to_db(dat = df %>%
                                   # Filter out reference documents that are already in CvTDB
                                   dplyr::filter(!(document_type == 2 & !is.na(fk_document_id))) %>%
                                   dplyr::select(dplyr::any_of(tbl_fields)),
                                 tblName="documents",
                                 overwrite=FALSE, 
                                 append=TRUE) 
  if(is.null(db_push_rs) || is.na(db_push_rs)){
    message("Issue pushing documents table.")
    browser()
  }
  # Match back new document records
  doc_sheet_list$Documents = match_cvt_doc_to_db_doc(df = df %>%
                                                       dplyr::select(-fk_document_id))
  
  # Check if any did not match to previously pushed document entry
  if(any(is.na(df$fk_document_id))){
    message("Error mapping to pushed document entry...")
    browser()
  }
  
  # Push Document Lineage Linkages
  doc_lineage = df %>%
    dplyr::select(fk_doc_id = fk_document_id, 
                  relationship_type = document_type)
  # Add parent doc (document_type == 1)
  doc_lineage$fk_parent_doc_id = doc_lineage$fk_doc_id[doc_lineage$relationship_type == 1]
  doc_lineage = doc_lineage %>%
    dplyr::filter(fk_parent_doc_id != fk_doc_id) %>%
    dplyr::mutate(relationship_type = dplyr::case_when(
      relationship_type == 2 ~ "Reference Document",
      relationship_type == 3 ~ "Supplemental Document",
      relationship_type == 4 ~ "Study Methods Document",
      TRUE ~ as.character(relationship_type)
    ))
  
  message("...pushing to Document Lineage table")
  browser()
  db_push_tbl_to_db(dat=doc_lineage,
                    tblName="documents_lineage",
                    overwrite=FALSE, 
                    append=TRUE)
  
  if(load_doc_sheet_only){
    # Export loaded template log
    output_dir = file.path("output", "Document Loading", cvt_dataset)
    if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    # Write export file  
    writexl::write_xlsx(doc_sheet_list, path=paste0(output_dir,"/", 
                                                    basename(to_load$filename[i]) %>% gsub(".xlsx", "", .), 
                                                    "_loaded_", format(Sys.time(), "%Y%m%d"), 
                                                    ".xlsx"))
    message("Finished load of document sheet only...")
    return(df)
  }
  
  return(df)
}