qc_to_db <- function(schema = 'cvt',
                     log_path = "output/qc_to_db_log.xlsx",
                     qc_dataset = "CVT_dermal",
                     col_exclude = c()
                     ) {
  
  # Set default variables
  load_doc_sheet_only = FALSE
  apiKey = Sys.getenv("apiKey")
  baseurl = Sys.getenv("baseurl")
  dsID = Sys.getenv("qc_dsID")
  doc_dsID = Sys.getenv("doc_dsID")
  load_mode = "QC"
  
  loaded_jira_docs = db_query_cvt(paste0("SELECT qc_clowder_template_id FROM cvt.documents ",
                                         "WHERE qc_jira_ticket IS NOT NULL"))
  # Pull dataset ticket templates and filter to those not loaded
  to_load = pull_clowder_files_to_load(dsID, baseurl, apiKey, curation_set_tag=qc_dataset, metadata_filter_tag=NULL) %>%
    dplyr::filter(!clowder_id %in% loaded_jira_docs$qc_clowder_template_id)
  
  # Load inputs for needed load
  cvt_template = get_cvt_template("input/CvT_data_template_articles.xlsx")
  tbl_field_list = db_query_cvt(paste0("SELECT table_name, column_name FROM information_schema.columns WHERE table_schema='", schema,"'"))
  clowder_file_list = clowder_get_dataset_files(dsID=doc_dsID, baseurl=baseurl, apiKey=apiKey)
  
  # Loop through Clowder files to load
  for(i in seq_len(nrow(to_load))){
    message("Pushing file (", i, "/", nrow(to_load),"): ", toString(to_load[i,c("jira_ticket", "filename")]), "...", Sys.time())
    f = to_load$filename[i]
    #############################################################################################
    ### Start of generic logic used for both Load and QC
    ### TODO Make generic function for these overall steps between Load and QC
    #############################################################################################
    doc_sheet_list = load_file_from_api(url = paste0(baseurl,"/api/files/",to_load$clowder_id[i],"/blob"),
                                        headers = c(`X-API-Key` = apiKey),
                                        mode = "wb",
                                        file_type = "xlsx")
    
    # Select Template Sheets (remove any excess sheets)
    doc_sheet_list = doc_sheet_list[names(cvt_template)[names(cvt_template) %in% names(doc_sheet_list)]]
    
    if (!validate_cvt(df=doc_sheet_list, df_identifier = f, log_path=log_path)) {
      stop("Validation failed, exiting.")
    }
    
    # Check for template with only Documents sheet
    if(length(doc_sheet_list) == 1 & all(names(doc_sheet_list) == "Documents")){
      load_doc_sheet_only = TRUE
    } 
    
    # Rename "original" fields
    doc_sheet_list = set_original_fields(sheet_list=doc_sheet_list, schema = schema)
    
    # Check for fields not in database tables - need to add
    for(s in names(doc_sheet_list)){
      # Set names to lowercase (case of dermal_dose_vehicle_pH should be dermal_dose_vehicle_ph)
      names(doc_sheet_list[[s]]) <- tolower(names(doc_sheet_list[[s]]))
      new_names = names(doc_sheet_list[[s]])[
        !names(doc_sheet_list[[s]]) %in% 
          tbl_field_list$column_name[
            tbl_field_list$table_name == tolower(s)]] %>%
        .[!grepl("^fk_|_original$|document_type|qc_push_category|qc_reviewer_lanid", .)]
      if(length(new_names)){
        message("New fields to add to database for table ", s, ": ")
        cat(paste0("- ", new_names), sep="\n")
        stop("Add new fields to table...")
      }
    }
    # Update database dictionaries and get dictionary foreign keys    
    doc_sheet_list = get_dict_update_ids(sheet_list=doc_sheet_list, schema = schema)
    
    if(!load_doc_sheet_only){
      # Rename foreign key fields as needed
      doc_sheet_list$Studies = doc_sheet_list$Studies %>%
        dplyr::rename(fk_dosed_chemical_id=fk_chemicals_id)
      doc_sheet_list$Series = doc_sheet_list$Series %>%
        dplyr::rename(fk_analyzed_chemical_id=fk_chemicals_id)  
    }
    
    # Match to document records in CvTdb, if available
    doc_sheet_list$Documents = match_cvt_doc_to_db_doc(df = doc_sheet_list$Documents)
    
    # Add Clowder data provenance
    doc_sheet_list$Documents = doc_sheet_list$Documents %>%
      dplyr::mutate(qc_jira_ticket = to_load$jira_ticket[i],
                    qc_set_tag = to_load$curation_set_tag[i],
                    qc_clowder_template_id = to_load$clowder_id[i])
    # Add field if not present
    if(!"clowder_file_id" %in% names(doc_sheet_list$Documents)){
      doc_sheet_list$Documents$clowder_file_id = as.character(NA)
    }
    # Match to Clowder documents where clowder_file_id is NA
    if(any(is.na(doc_sheet_list$Documents$clowder_file_id))){
      doc_sheet_list$Documents = clowder_match_docs(df=doc_sheet_list$Documents,
                                                    dsID=doc_dsID,
                                                    baseurl=baseurl,
                                                    apiKey=apiKey,
                                                    clowder_file_list=clowder_file_list)  
    }

################################################################################    
    # get/set ID values and foreign key relations between sheets
    # Account for whether it is a load vs. QC based on "QC_"
    tbl_id_list <- get_next_tbl_id(schema)
    fk_map = lapply(names(doc_sheet_list), function(sheet){
      n_id = tbl_id_list[[tolower(sheet)]]
      
      if(load_mode == "QC"){
        tmp1 = doc_sheet_list[[sheet]] %>%
          dplyr::select(id) %>%
          dplyr::filter(!grepl("QC", id)) %>%
          dplyr::mutate(sheet = !!sheet,
                        fk_id = as.character(id),
                        id = as.character(id))
        
        tmp2 = doc_sheet_list[[sheet]] %>%
          dplyr::filter(!id %in% tmp1$id)
        if(nrow(tmp2)){
          tmp2 = tmp2 %>%
            dplyr::select(id) %>%
            dplyr::mutate(id = id %>%
                            gsub("QC_", "", .) %>%
                            as.numeric()) %>%
            dplyr::arrange(id) %>%
            dplyr::mutate(
              id = paste0("QC_", id),
              sheet = !!sheet,
              fk_id = as.character(seq(n_id, (n_id + dplyr::n() - 1)))
            )
          return(dplyr::bind_rows(tmp1, tmp2))
        }
        return(tmp1)
      } else {
        # TODO Handle case for load and case of documents fk_ column with previous database matches
        if(sheet == "Documents"){
          # fk_document_id
          
        } else {
          tmp = doc_sheet_list[[sheet]] %>%
            dplyr::select(id) %>%
            dplyr::mutate(sheet = !!sheet,
                          fk_id = seq(n_id, (n_id + dplyr::n() - 1))
            )  
        }
      }
    }) %>%
      dplyr::bind_rows() %>%
      # Filter out those that do not change (mainly for QC load_mode)
      dplyr::filter(id != fk_id)
    
    for(sheet in unique(fk_map$sheet)){
      key_map = fk_map %>%
        dplyr::filter(sheet == !!sheet) %>%
        dplyr::select(-sheet)
      
      # Map id to fk_id from fk_map for sheet
      doc_sheet_list[[sheet]] = doc_sheet_list[[sheet]] %>%
        dplyr::left_join(key_map,
                         by = "id") %>%
        dplyr::mutate(fk_id = dplyr::case_when(
          is.na(fk_id) ~ id,
          TRUE ~ fk_id
        )) %>%
        dplyr::select(-id) %>%
        dplyr::rename(id = fk_id)
      
      # Create foreign_key table-field pair map
      fk_list = switch(sheet,
                       "Documents" = c("Studies", "fk_reference_document"),
                       "Studies" = c("Series", "fk_study_id"),
                       "Subjects" = c("Series", "fk_subject_id"),
                       "Series" = c("Conc_Time_Values", "fk_series_id"))
      
      # Map foreign key fields with table-field pair
      if(!is.null(fk_list)){
        doc_sheet_list[[fk_list[1]]] = doc_sheet_list[[fk_list[1]]] %>%
          dplyr::left_join(key_map,
                           by = dplyr::join_by(!!fk_list[2] == id)) %>%
          dplyr::mutate(fk_id = dplyr::case_when(
            is.na(fk_id) ~ !!rlang::sym(fk_list[2]),
            TRUE ~ fk_id
          )) %>%
          dplyr::select(-dplyr::any_of(c(fk_list[2]))) %>%
          dplyr::rename(!!fk_list[2] := fk_id)
      }
    }
    
    # Check all foreign key fields. Must be a numeric value (not NA)
    for(sheet in names(doc_sheet_list)){
      tmp = doc_sheet_list[[sheet]] %>%
        dplyr::select(dplyr::starts_with("fk_")) %>%
        # Exclude optional foreign keys
        dplyr::select(-dplyr::any_of(c("fk_reference_document_id"))) %>%
        # Set to numeric, producing NA's where not numeric
        dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
      if(nrow(tmp)){
        # Compare dataframe without NA values versus original
        if(nrow(na.omit(tmp)) != nrow(tmp)){
          stop("Foreign key missing in ", sheet, " sheet")
        }
      }
    }
    
    #############################################################################################
    ### End of generic logic used for both Load and QC
    #############################################################################################
    
    # Convert all qc_status and qc_flags to lowercase, for simpler comparison
    for (sheet_name in names(doc_sheet_list)) {
      doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
        dplyr::mutate(
          qc_status = tolower(qc_status),
          qc_flags = tolower(qc_flags)
        )
    }
    
    # Get created_by as Documents "qc_reviewer_lanid" for all add and update processes
    qc_user = doc_sheet_list$Documents$qc_reviewer_lanid %>% 
      # Remove NA values
      .[!is.na(.)] %>%
      unique() %>%
      toString()
    
    # Set QC qc_push_category to determine database action by status and flags
    doc_sheet_list = lapply(doc_sheet_list, function(sheet){
      sheet %>%
        # Categorize each record based on 4 conditions of remove, update, add, or ignore
        dplyr::mutate(
          qc_push_category = dplyr::case_when(
            qc_status == "fail" ~ "Remove",
            qc_flags == "modified" ~ "Update",
            qc_flags == "new entry" | grepl("split entry", qc_flags) ~ "Add",
            TRUE ~ "Pass"
          ),
          # Set created_by
          created_by = qc_user
        )
    }) %T>% {
      names(.) <- names(doc_sheet_list)
    }
    
    # Export prepped QC load template before pushing results (helps with restarting/checking)
    output_dir = file.path("output", "Document QC Export", qc_dataset)
    if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    # Write export file  
    writexl::write_xlsx(doc_sheet_list, path=paste0(output_dir,"/", 
                                                    basename(to_load$filename[i]) %>% gsub(".xlsx", "", .), 
                                                    "_loaded_", format(Sys.time(), "%Y%m%d"), 
                                                    ".xlsx"))
    
    # TODO Ensure connections between split entry records are clear/captured
    # before deleting parent record (i.e. qc_notes establish the parent ID)
    
    # Delete/remove records in specific order to handle cascade needs due to foreign key connections
    message("Removing records...")
    for(sheet_name in c("Conc_Time_Values", "Series", "Subjects", "Studies", "Documents")){
      # Remove these ids from the database
      qc_remove_record(df = doc_sheet_list[[sheet_name]] %>%
                         dplyr::filter(qc_push_category == "Remove") %>%
                         dplyr::select(id, qc_notes, qc_flags),
                       tbl_name = sheet_name)
    }
    
################################################################################    
    # If document already present, merge field values
    # Do this after qc_remove_record so that any removed cases are ignored
    # Get documents table fields
    tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == "documents"] %>%
      .[!. %in% col_exclude]
    doc_in_db = db_query_cvt(paste0("SELECT * FROM cvt.documents where id = ",
                                    doc_sheet_list$Documents$fk_document_id[!is.na(doc_sheet_list$Documents$fk_document_id)]))
    temp_doc = doc_sheet_list$Documents %>%
      # Filter out NA fields (to be filled by database document fields)
      .[ , colSums(is.na(.)) < nrow(.)] %>%
      dplyr::select(-id, -fk_document_id)
    
    # Combine fields from template with fields from document entry
    doc_in_db = doc_in_db %>%
      dplyr::select(any_of(
        names(doc_in_db)[!names(doc_in_db) %in% names(temp_doc)]
      )) %>%
      cbind(., temp_doc) %>%
      # Remove versioning, handled by database audit triggers
      dplyr::select(-rec_create_dt, -version) %>%
      # Order columns by database table order
      dplyr::select(id, any_of(tbl_fields), document_type)
    
    # Update database entry for document
    db_update_tbl(df=doc_in_db %>%
                    dplyr::select(-document_type),
                  tblName = "documents")
    
    # Remove entries already in database that were updated
    doc_sheet_list$Documents = doc_sheet_list$Documents[is.na(doc_sheet_list$Documents$fk_document_id)]
    
    doc_sheet_list = doc_sheet_list %>%
      purrr::compact()
    
################################################################################    
    # Filter only to records that are "Add"
    qc_add_record(df = purrr::map(doc_sheet_list, function(df){ dplyr::filter(df, qc_push_category == "Add")}),
                  tbl_field_list=tbl_field_list, 
                  load_doc_sheet_only=load_doc_sheet_only,
                  col_exclude=col_exclude)
    
    
    # Select and iterate through "Pass" QC Category record updates
    df = purrr::map(doc_sheet_list, function(df){ dplyr::filter(df, qc_push_category == "Pass")}) %>%
      .[sapply(., function(x) dim(x)[1]) > 0]
    
    if(length(df)){
      for(sheet_name in names(df)){
        message("...pushing 'Pass' records for ", sheet_name, " sheet")
        tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == tolower(sheet_name)]
        
        # Update unchanged records to qc_status = "pass"
        db_update_tbl(df = doc_sheet_list[[sheet_name]] %>%
                        dplyr::filter(qc_push_category == "Pass") %>%
                        dplyr::select(id, created_by) %>%
                        dplyr::mutate(qc_status = "pass",
                                      qc_notes = "QC pass without changes"),
                      tblName = sheet_name)  
      }
    }
    
    # Select and iterate through "Pass" QC Category record updates
    df = purrr::map(doc_sheet_list, function(df){ dplyr::filter(df, qc_push_category == "Update")}) %>%
      .[sapply(., function(x) dim(x)[1]) > 0]
    
    if(length(df)){
      for(sheet_name in names(df)){
        message("...pushing 'Update' records for ", sheet_name, " sheet")
        tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == tolower(sheet_name)]
        
        # Update unchanged records to qc_status = "pass"
        db_update_tbl(df = doc_sheet_list[[sheet_name]] %>%
                        dplyr::filter(qc_push_category == "Update") %>%
                        dplyr::mutate(qc_status = "pass") %>%
                        dplyr::select(dplyr::any_of(tbl_fields)), 
                      tblName = sheet_name)
      }
    }
    
    # TODO: Run normalization of updated/added records
  }
}