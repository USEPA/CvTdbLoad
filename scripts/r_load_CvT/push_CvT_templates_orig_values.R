# Script to push CvT Original Extracted Values to CvT from Clowder
# This decouples the normalization from loading original values first.
# Created by: Jonathan Taylor Wall
# Created Date: 2023-12-14

load_cvt_templates_to_db <- function(
    schema = 'cvt',
    log_path = "output/load_required_fields_log.xlsx",
    cvt_dataset = NULL,
    col_exclude = c("created_by", "rec_create_dt", "qc_status", "qc_flags", "qc_notes", "version")
  ){
  
  apiKey = Sys.getenv("apiKey")
  baseurl = Sys.getenv("baseurl")
  dsID = Sys.getenv("file_dsID")
  doc_dsID = Sys.getenv("doc_dsID")
  load_mode = "curation"
  
  # Query already loaded Jira tickets
  loaded_jira_docs = db_query_cvt(paste0("SELECT clowder_template_id, jira_ticket FROM cvt.documents ",
                                         "WHERE jira_ticket IS NOT NULL"))
  
  # Pull dataset ticket templates and filter to those not loaded
  to_load = pull_clowder_files_to_load(dsID, baseurl, apiKey, curation_set_tag=cvt_dataset, metadata_filter_tag=NULL) %>%
    dplyr::filter(!clowder_id %in% loaded_jira_docs$clowder_template_id,
                  !jira_ticket %in% loaded_jira_docs$jira_ticket,
                  grepl("_final\\.xlsx", filename))
  
  #Check for duplicates in ticket. Each ticket should only have 1 final
  dups = to_load %>%
    dplyr::filter(duplicated(jira_ticket))
  if(nrow(dups)){
    stop("Duplicate 'final' QC templates found for tickets: ", toString(dups$jira_ticket))
  }
  
  # Only process if Clowder File records pulled
  if(nrow(to_load)){
    # Load inputs for needed load
    cvt_template = get_cvt_template("input/CvT_data_template_articles.xlsx")
    tbl_field_list = db_query_cvt(paste0("SELECT table_name, column_name FROM information_schema.columns WHERE table_schema='", schema,"'"))
    clowder_file_list = clowder_get_dataset_files(dsID=doc_dsID, baseurl=baseurl, apiKey=apiKey)
    # Loop through Clowder files to load
    for(i in seq_len(nrow(to_load))){
      message("Pushing file (", i, "/", nrow(to_load),"): ", toString(to_load[i,c("jira_ticket", "filename")]), "...", Sys.time())
      # Filename
      f = to_load$filename[i]
      # Boolean of whether to only load document sheet
      load_doc_sheet_only = FALSE
      
      # Create/clear log entry for filename
      log_CvT_doc_load(f, m=NULL, reset=TRUE, 
                       log_path = log_path)
      
      # Pull temp file to process
      doc_sheet_list = load_file_from_api(url = paste0(baseurl,"/api/files/",to_load$clowder_id[i],"/blob"),
                                          headers = c(`X-API-Key` = apiKey),
                                          mode = "wb",
                                          file_type = "xlsx")
      
      # Select Template Sheets
      doc_sheet_list = doc_sheet_list[names(cvt_template)[names(cvt_template) %in% names(doc_sheet_list)]]
      
      # Validation using the standard curation template
      if (!validate_cvt(df=doc_sheet_list, df_identifier = f, log_path=log_path, ignore_qc = TRUE)) {
        stop("Validation failed, exiting.")
      }
      
      # Remove empty rows and columns (all NA values)
      doc_sheet_list = lapply(names(doc_sheet_list), function(s){
        doc_sheet_list[[s]] = doc_sheet_list[[s]][!apply(is.na(doc_sheet_list[[s]]), 1, all),]
        doc_sheet_list[[s]] = doc_sheet_list[[s]][!sapply(doc_sheet_list[[s]], function(x) all(is.na(x)))]
      }) %T>% {
        names(.) <- names(doc_sheet_list)
      }
      
      # Fill in missing template fields
      doc_sheet_list = lapply(names(doc_sheet_list), function(s){
        
        doc_sheet_list[[s]][, names(cvt_template[[s]])[!names(cvt_template[[s]]) %in% names(doc_sheet_list[[s]])]] <- as.character(NA)
        return(doc_sheet_list[[s]])
      }) %T>% {
        names(.) <- names(doc_sheet_list)
      }
      
      # ##########################################################################
      # ### Default extracted to 3 if submitted as NA
      # doc_sheet_list$Documents$extracted[is.na(doc_sheet_list$Documents$extracted)] = 3
      # ##########################################################################
      
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
          .[!grepl("^fk_|_original$|document_type", .)]
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
        # Add Conc_Time_Values id column for ID mapping
        doc_sheet_list$Conc_Time_Values = doc_sheet_list$Conc_Time_Values %>%
          dplyr::mutate(id = 1:dplyr::n())
      }
      
      ###########################################################################
      ### Parse the where clause to search by pmid, other_study_identifier, or doi
      ###########################################################################
      # Check for duplicate docs within the template
      if(any(duplicated(doc_sheet_list$Documents$pmid[!is.na(doc_sheet_list$Documents$pmid)]))) stop("Duplicate PMID values found in template...")
      if(any(duplicated(doc_sheet_list$Documents$other_study_identifier[!is.na(doc_sheet_list$Documents$other_study_identifier)]))) stop("Duplicate other_study_identifier values found in template...")
      
      # Match to document records in CvTdb, if available
      doc_sheet_list$Documents = match_cvt_doc_to_db_doc(df = doc_sheet_list$Documents)
      doc_sheet_list$Documents = doc_sheet_list$Documents %>%
        dplyr::mutate(qc_push_category = dplyr::case_when(
          is.na(fk_document_id) ~ "Add",
          TRUE ~ "Update"
        ))
      
      ###########################################################################
      ### Push Documents Sheet to CvT
      ###########################################################################
      if(!"jira_ticket" %in% names(doc_sheet_list$Documents)){
        doc_sheet_list$Documents$jira_ticket = NA
      }
      
      # Add Clowder data provenance for extraction document
      doc_sheet_list$Documents = doc_sheet_list$Documents %>%
        dplyr::mutate(
          jira_ticket = dplyr::case_when(
            document_type == 1 ~ to_load$jira_ticket[i],
            TRUE ~ jira_ticket),
          curation_set_tag = dplyr::case_when(
            document_type == 1 ~ to_load$curation_set_tag[i],
            TRUE ~ NA
          ),
          clowder_template_id = dplyr::case_when(
            document_type == 1 ~ to_load$clowder_id[i],
            TRUE ~ NA
          ))
      
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
          tmp = doc_sheet_list[[sheet]] %>%
            dplyr::select(id) %>%
            dplyr::mutate(sheet = !!sheet,
                          fk_id = seq(n_id, (n_id + dplyr::n() - 1)),
                          id = as.numeric(id)
            )
        }
      }) %>%
        dplyr::bind_rows() %>%
        # Filter out those that do not change (mainly for QC load_mode)
        dplyr::filter(id != fk_id)
      
      # Update fk_map with Document entries that already exist
      fk_doc_id_exists = doc_sheet_list$Documents %>%
        dplyr::select(id, fk_id = fk_document_id) %>%
        dplyr::filter(!is.na(fk_id)) %>%
        dplyr::mutate(sheet = "Documents",
                      id = as.numeric(id)) %>%
        dplyr::bind_rows()
      
      # Add fk_map entries that are new
      fk_doc_id_exists = fk_doc_id_exists %>%
        dplyr::bind_rows(fk_map %>%
                           dplyr::filter(sheet == "Documents",
                                         !id %in% fk_doc_id_exists$id))
      
      # Filter out auto-generated and fill in existing
      fk_map = fk_map %>%
        dplyr::filter(sheet != "Documents") %>%
        dplyr::bind_rows(fk_doc_id_exists)
      
      for(sheet in unique(fk_map$sheet)){
        key_map = fk_map %>%
          dplyr::filter(sheet == !!sheet) %>%
          dplyr::select(-sheet)
        
        # Map id to fk_id from fk_map for sheet
        doc_sheet_list[[sheet]] = doc_sheet_list[[sheet]] %>%
          dplyr::mutate(id = as.numeric(id)) %>%
          dplyr::left_join(key_map,
                           by = "id") %>%
          dplyr::mutate(fk_id = dplyr::case_when(
            is.na(fk_id) ~ id,
            TRUE ~ fk_id
          )) %>%
          dplyr::select(-id) %>%
          dplyr::rename(id = fk_id) %>%
          dplyr::mutate(id = as.numeric(id))
        
        # Create foreign_key table-field pair map
        fk_list = switch(sheet,
                         "Documents" = c("Studies", "fk_reference_document_id"),
                         "Studies" = c("Series", "fk_study_id"),
                         "Subjects" = c("Series", "fk_subject_id"),
                         "Series" = c("Conc_Time_Values", "fk_series_id"))
        
        # Map foreign key fields with table-field pair
        if(!is.null(fk_list)){
          if(fk_list[1] %in% names(doc_sheet_list)){
            doc_sheet_list[[fk_list[1]]] = doc_sheet_list[[fk_list[1]]] %>%
              dplyr::mutate(!!fk_list[2] := as.numeric(!!rlang::sym(fk_list[2]))) %>%
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
      }
      
      # Handle case where a "New" record already has an entry in the documents table
      # Map to existing Document ID
      doc_sheet_list$Documents$id[!is.na(doc_sheet_list$Documents$fk_document_id)] = doc_sheet_list$Documents$fk_document_id[!is.na(doc_sheet_list$Documents$fk_document_id)]
      
      # Check all foreign key fields. Must be a numeric value (not NA)
      for(sheet in names(doc_sheet_list)){
        tmp = doc_sheet_list[[sheet]] %>%
          dplyr::select(dplyr::starts_with("fk_")) %>%
          # Exclude optional foreign keys
          dplyr::select(-dplyr::any_of(c("fk_reference_document_id", "fk_document_id"))) %>%
          # Set to numeric, producing NA's where not numeric
          dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
        if(nrow(tmp)){
          # Compare dataframe without NA values versus original
          if(nrow(na.omit(tmp)) != nrow(tmp)){
            stop("Foreign key missing in ", sheet, " sheet")
          }
        }
      }
      
      # Set QC qc_push_category to determine database action by status and flags
      doc_sheet_list = lapply(doc_sheet_list, function(sheet){
        sheet %>%
          # Add tag to add records to database
          dplyr::mutate(qc_push_category = "Add") %>%
          return()
      }) %T>% {
        names(.) <- names(doc_sheet_list)
      }
      
      # Export loaded template log
      output_dir = file.path("output", "Document Loading", to_load$curation_set_tag[i])
      if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      
      # Write export file  
      writexl::write_xlsx(doc_sheet_list, path=paste0(output_dir,"/", 
                                                      basename(to_load$filename[i]) %>% gsub(".xlsx", "", .), 
                                                      "_loaded_", format(Sys.time(), "%Y%m%d"), 
                                                      ".xlsx"))
      
      ################################################################################    
      # If document already present, merge field values
      if(!all(is.na(doc_sheet_list$Documents$fk_document_id))){
        # Get documents table fields
        tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == "documents"] %>%
          .[!. %in% col_exclude]
        doc_in_db = db_query_cvt(paste0("SELECT * FROM cvt.documents where id in (",
                                        toString(doc_sheet_list$Documents$fk_document_id[!is.na(doc_sheet_list$Documents$fk_document_id)]),
                                        ")"
        ))
        
        # Update document-by-document so only updates non-missing fields
        # Doesn't overwrite database field with NA/NULL
        for(id in doc_in_db$id){
          temp_doc = doc_sheet_list$Documents %>%
            dplyr::filter(fk_document_id %in% !!id) %>%
            # Filter out NA fields (to be filled by database document fields)
            .[ , colSums(is.na(.)) < nrow(.)] %>%
            dplyr::select(-fk_document_id) %>%
            dplyr::mutate(id = as.numeric(id))
          
          # Combine fields from template with fields from document entry
          doc_in_db_push = doc_in_db %>%
            dplyr::filter(id %in% !!id) %>%
            dplyr::select(any_of(
              names(doc_in_db)[!names(doc_in_db) %in% names(temp_doc)[!names(temp_doc) %in% "id"]]
            )) %>%
            dplyr::left_join(temp_doc,
                             by="id") %>%
            # dplyr::bind_cols(temp_doc) %>%
            # Remove versioning, handled by database audit triggers
            dplyr::select(-rec_create_dt, -version) %>%
            # Order columns by database table order
            dplyr::select(id, any_of(tbl_fields), document_type)
          
          # Update database entry for document
          db_update_tbl(df=doc_in_db_push %>%
                          dplyr::select(-document_type),
                        tblName = "documents")
        } 
      }
      ################################################################################    
      # Filter only to records that are "Add"
      # Select and iterate through "Pass" QC Category record updates
      qc_add_record(df = doc_sheet_list,
                    tbl_field_list=tbl_field_list, 
                    load_doc_sheet_only=load_doc_sheet_only,
                    col_exclude=col_exclude)
    }
  }
  
  message("Done. - ", Sys.time())
}

