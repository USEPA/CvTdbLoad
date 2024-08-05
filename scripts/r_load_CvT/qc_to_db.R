qc_to_db <- function(files, schema) {
  col_exclude = c()
  log_path <- "output/qc_to_db_log.xlsx"
  load_doc_sheet_only = FALSE
  apiKey = Sys.getenv("apiKey")
  baseurl = Sys.getenv("baseurl")
  dsID = Sys.getenv("qc_dsID")
  doc_dsID = Sys.getenv("doc_dsID")
  qc_dataset = "CVT_dermal"
  schema = "cvt"
  log_path = "output/load_required_fields_log.xlsx"
  load_mode = "QC"
  
  
  loaded_jira_docs = db_query_cvt(paste0("SELECT clowder_template_id FROM cvt.documents ",
                                         "WHERE qc_jira_ticket IS NOT NULL"))
  # Pull dataset ticket templates and filter to those not loaded
  to_load = pull_clowder_files_to_load(dsID, baseurl, apiKey, curation_set_tag=qc_dataset, metadata_filter_tag=NULL) %>%
    dplyr::filter(!clowder_id %in% loaded_jira_docs$clowder_template_id)
  
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
    
    # Skipped push_CvT_templates_orig_values.R logic to check if doc entries already exist because for QC they will
    
    # Set boolean to update doc db information
    update_doc_in_db = TRUE
    
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
    
    # TODO Add logic for update_doc_in_db from push_CvT_templates_orig_values.R
    # Reconcile combining/updating document entries that exist in the database
    
    # TODO Add logic to get/set ID values and foreign key relations between sheets
    # Account for whether it is a load vs. QC based on "QC_"
    tbl_id_list <- get_next_tbl_id(schema)
    fk_map = lapply(names(doc_sheet_list), function(sheet){
      n_id = tbl_id_list[[tolower(sheet)]]
      
      if(load_mode == "QC"){
        tmp1 = doc_sheet_list[[sheet]] %>%
          dplyr::select(id) %>%
          dplyr::filter(!grepl("QC", id)) %>%
          dplyr::mutate(sheet = !!sheet,
                        fk_id = as.character(id))
        
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
      dplyr::bind_rows()
    
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
          )
        )
    }) %T>% {
      names(.) <- names(doc_sheet_list)
    }
    
    # TODO Set created_by as Documents "qc_reviewer_lanid" for all add and update processes
    
    # TODO Ensure connections between split entry records are clear/captured
    # before deleting parent record (i.e. qc_notes establish the parent ID)
    
    # Delete/remove records in specific order to handle cascade needs due to foreign key connections
    for(sheet_name in c("Conc_Time_Values", "Series", "Subjects", "Studies", "Documents")){
      # Remove these ids from the database
      qc_remove_record(df = doc_sheet_list[[sheet_name]] %>%
                         dplyr::filter(qc_push_category == "Remove") %>%
                         dplyr::select(id, qc_notes, qc_flags),
                       tbl_name = sheet_name)
    }
    
    # TODO Consider taking Add outside of loop and having subset doc_sheet_list of
    # all records that need to be added - process linearly as if they're a new template load
    # Then match back local ID linkages for records that need them
    # Add these new rows to the database
    qc_add_record(df = doc_sheet_list[[sheet_name]] %>%
                    dplyr::filter(qc_push_category == "Add"),
                  tbl_field_list=tbl_field_list, 
                  load_doc_sheet_only=load_doc_sheet_only)
    
    
    # Interate through each sheet and qc_push_category to perform specific actions in the database
    for (sheet_name in names(doc_sheet_list)) {
      
      tbl_fields = tbl_field_list$column_name[tbl_field_list$table_name == tolower(sheet_name)]
      
      # Update unchanged records to qc_status = "pass"
      db_update_tbl(df = doc_sheet_list[[sheet_name]] %>%
                      dplyr::filter(qc_push_category == "Pass") %>%
                      dplyr::select(id) %>%
                      dplyr::mutate(qc_status = "pass",
                                    qc_notes = "QC pass without changes") %>%
                      dplyr::select(dplyr::any_of(tbl_fields)), 
                    tblName = sheet_name)
      
      # TODO Append updated ID values to doc_sheet_list for foreign key 
      # substitutions of QC_# values
      
      # TODO: Add check/select for only fields in database table
      # Update unchanged records to qc_status = "pass"
      db_update_tbl(df = doc_sheet_list[[sheet_name]] %>%
                      dplyr::filter(qc_push_category == "Update") %>%
                      dplyr::mutate(qc_status = "pass") %>%
                      dplyr::select(dplyr::any_of(tbl_fields)), 
                    tblName = sheet_name)
      
      # TODO: Run normalization of updated/added records
    }
  }
}