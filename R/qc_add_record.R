#' @title qc_add_record
#' @description Function to add record from QC template 
#' @param df Input dataframe of field values to add to database table
#' @param tbl_field_list Dataframe of CvT tables and fields
#' @param load_doc_sheet_only Boolean whether just to add document sheet only
#' @param col_exclude List of columns to exclude from database pushes
#' @export
#' @return None, SQL statements are submitted
qc_add_record <- function(df, tbl_field_list, load_doc_sheet_only, col_exclude){
  
  # Remove empty rows and columns (all NA values)
  df = lapply(names(df), function(s){
    df[[s]] = df[[s]][!apply(is.na(df[[s]]), 1, all),]
    df[[s]] = df[[s]][!sapply(df[[s]], function(x) all(is.na(x)))]
  }) %T>% {
    names(.) <- names(df)
  } %>% 
    purrr::compact()
  
  if(!length(df)){
    message("...no records to add for QC template...")
    return()
  }
  
  # Adapted from push_CvT_templates_orig_values.R
  if("Documents" %in% names(df)){
    if(nrow(df$Documents)){
      message("...pushing to Documents table")
      
      ### Default extracted to 3 if submitted as NA
      df$Documents$extracted[is.na(df$Documents$extracted)] = 3
      
      # Get documents table fields
      tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == "documents"] %>%
        .[!. %in% col_exclude]
      db_push_rs = db_push_tbl_to_db(dat = df$Documents %>%
                                       dplyr::filter(is.na(fk_document_id)) %>%
                                       dplyr::select(dplyr::any_of(tbl_fields)),
                                     tblName="documents",
                                     overwrite=FALSE, 
                                     append=TRUE) 
      if(is.null(db_push_rs) || is.na(db_push_rs)){
        message("Issue pushing documents table.")
        browser()
      }
      
      # Push Document Lineage Linkages
      doc_lineage = df$Documents %>%
        dplyr::select(fk_doc_id = id, 
                      relationship_type = document_type)
      
      # Add parent doc (document_type == 1)
      doc_lineage$fk_parent_doc_id = doc_lineage$fk_doc_id[doc_lineage$relationship_type == 1]
      
      # Pull existing doc lineage
      existing_lineage = db_query_cvt(paste0("SELECT CONCAT(fk_parent_doc_id, '_', fk_doc_id) as lineage_key ",
                                             "FROM cvt.documents_lineage ",
                                             "WHERE fk_parent_doc_id in (", toString(unique(doc_lineage$fk_parent_doc_id)), 
                                             ") OR fk_doc_id in (" , toString(unique(doc_lineage$fk_doc_id)), ")"))
      
      doc_lineage = doc_lineage %>%
        dplyr::mutate(lineage_key = paste0(fk_parent_doc_id, "_", fk_doc_id)) %>%
        dplyr::filter(fk_parent_doc_id != fk_doc_id,
                      # Filter out already existing doc lineage linkages
                      !lineage_key %in% existing_lineage$lineage_key) %>%
        dplyr::select(-lineage_key)
      
      if(nrow(doc_lineage)){
        # Prep and filter doc lineage
        doc_lineage = doc_lineage %>%
          dplyr::mutate(relationship_type = dplyr::case_when(
            relationship_type == 2 ~ "Reference Document",
            relationship_type == 3 ~ "Supplemental Document",
            relationship_type == 4 ~ "Study Methods Document",
            TRUE ~ as.character(relationship_type)
          ))
        
        message("...pushing to Document Lineage table")
        db_push_tbl_to_db(dat=doc_lineage,
                          tblName="documents_lineage",
                          overwrite=FALSE, 
                          append=TRUE)
      }
      
      # If true, only load documents sheet and return
      if(load_doc_sheet_only) return() 
    }
  }
  
  # Push sheets
  for(sheet in names(df)[!names(df) %in% c("Documents")]){
    # Filter to only those that have "Add" qc_push_category
    df[[sheet]] = df[[sheet]] %>%
      dplyr::filter(qc_push_category == "Add")
    
    if(nrow(df[[sheet]])){
      if(sheet == "Studies"){
        # Set extraction document ID
        df$Studies$fk_extraction_document_id = df$Documents$fk_document_id[df$Documents$document_type == 1]
      }
      
      # Get table fields
      tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == tolower(sheet)] %>%
        .[!. %in% col_exclude]
      message("...pushing records for ", sheet, " sheet")
      db_push_rs = db_push_tbl_to_db(dat=df[[sheet]] %>%
                                       dplyr::select(dplyr::any_of(tbl_fields)),
                                     tblName=tolower(sheet),
                                     overwrite=FALSE, 
                                     append=TRUE)
      if(is.null(db_push_rs) || is.na(db_push_rs)){
        message("Issue pushing ",sheet," table.")
        browser()
      }
    }
  }
}