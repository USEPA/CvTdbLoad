# Script to push CvT Extracted data to CvT from Clowder
# Created by: Jonathan Taylor Wall
# Created Date: 2023-11-28

################################################################################
# Boolean enforce chemical mapping
map_chemicals = FALSE
# Exclude versioning fields
col_exclude = c("created_by", "rec_create_dt", "qc_status", "qc_flags", "qc_notes", "version")

# Pull full list of Clowder folders to help with data provenance labeling
c_folders_list <- clowder_get_dataset_folders(dsID, baseurl, apiKey) %>%
  dplyr::mutate(foldername = foldername %>%
                  gsub("^\\/", "", .)) %>%
  tidyr::separate(foldername, into=c("curation_set_tag", "jira_ticket"), sep="/", 
                  extra="merge", fill="right")
# Pull full list of Clowder files in dataset with "normalized"  stem
c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey) %>%
  dplyr::filter(grepl("_normalized.xlsx", filename)) %>%
  dplyr::rename(jira_ticket=folders.name) %>%
  dplyr::left_join(c_folders_list,
                   by="jira_ticket")

# Check for jira_ticket subfolders with multiple normalized files
dups = c_files_list %>%
  dplyr::group_by(jira_ticket) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  dplyr::filter(n>1)

# Filter out jira_ticket subfolders with multiple normalized files
if(nrow(dups)){
  message("Jira tickets with multiple normalized files: ", toString(dups$jira_ticket))
  
  c_files_list = c_files_list %>%
    dplyr::filter(!jira_ticket %in% dups$jira_ticket)  
}

# Loop through Clowder files to load
for(i in seq_along(nrow(c_files_list))){
  message("Pushing file (", i, "/", nrow(c_files_list),"): ", toString(c_files_list[i,c("jira_ticket", "filename")]), "...", Sys.time())
  # Pull temp file to process
  doc_sheet_list = load_file_from_api(url = paste0(baseurl,"/api/files/",c_files_list$clowder_id[i],"/blob"),
                                      headers = c(`X-API-Key` = apiKey),
                                      mode = "wb",
                                      file_type = "xlsx")
  
  # Mutate column types to match database
  doc_sheet_list$Documents = doc_sheet_list$Documents %>%
    dplyr::mutate(dplyr::across(c(id, document_type, pmid, year), as.numeric),
           other_study_identifier = as.character(other_study_identifier))
  doc_sheet_list$Studies = doc_sheet_list$Studies %>%
    dplyr::mutate(dplyr::across(c(fk_reference_document_id, dermal_dose_vehicle_pH,
                    dermal_applied_area, aerosol_particle_diameter_mean,
                    aerosol_particle_diameter_gsd, aerosol_particle_density,
                    fk_administration_route), as.numeric))
  doc_sheet_list$Subjects = doc_sheet_list$Subjects %>%
    dplyr::mutate(dplyr::across(ends_with("units"), ~gsub("missing_units", NA, .))) %>%
    dplyr::mutate(dplyr::across(c(weight_estimated, weight_kg, height_cm), as.numeric))
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    dplyr::mutate(dplyr::across(c(x_min, x_max, y_min, y_max, loq, lod, radiolabeled, 
                    fk_study_id, fk_subject_id, n_subjects_in_series, conc_medium_id), as.numeric))
  doc_sheet_list$Conc_Time_Values = doc_sheet_list$Conc_Time_Values %>%
    dplyr::mutate(dplyr::across(c(time_original, time_hr, conc_original, conc_sd_original, conc_lower_bound_original, 
                    conc_upper_bound_original, conc, conc_sd, conc_lower_bound, conc_upper_bound
    ), as.numeric))
  
  ###########################################################################
  ### Parse the where clause to search by pmid, other_study_identifier, or doi
  ###########################################################################
  # Check for duplicate docs within the template
  if(any(duplicated(doc_sheet_list$Documents$pmid))) stop("Duplicate PMID valies found in template...")
  if(any(duplicated(doc_sheet_list$Documents$other_study_identifier))) stop("Duplicate other_study_identifier values found in template...")
  # Match to document records in CvTdb, if available
  doc_sheet_list$Documents = match_cvt_doc_to_db_doc(df = doc_sheet_list$Documents)
  
  # Skip processing if any document entries already present in the database
  if(any(!is.na(doc_sheet_list$Documents$fk_document_id))){
    message("...Document entry already in CvTdb. Need to plan how to handle.")
    next
  }
  
  ###########################################################################
  ### Push Documents Sheet to CvT
  ###########################################################################
  # Add Clowder data provenance
  doc_sheet_list$Documents = doc_sheet_list$Documents %>%
    dplyr::mutate(jira_ticket = c_files_list$jira_ticket[i],
                  curation_set_tag = c_files_list$curation_set_tag[i],
                  clowder_template_id = c_files_list$clowder_id[i])
  message("...pushing to Documents table")
  # Get documents table fields
  tbl_fields = db_query_cvt("SELECT * FROM cvt.documents limit 1") %>% 
    names() %>%
    .[!. %in% col_exclude]
  # names(doc_sheet_list$Documents)[!names(doc_sheet_list$Documents) %in% tbl_fields]
  browser()
  db_push_to_CvT(df=doc_sheet_list$Documents %>%
                   dplyr::select(dplyr::any_of(tbl_fields)),
              tblName="documents")
  
  # Match back new document records
  doc_sheet_list$Documents = match_cvt_doc_to_db_doc(df = doc_sheet_list$Documents %>%
                                                 dplyr::select(-fk_document_id))
  
  # Check if any did not match to previously pushed document entry
  if(any(is.na(doc_sheet_list$Documents$fk_document_id))){
    message("Error mapping to pushed document entry...")
    browser()
  }
  
  #####################################################################################
  #### Push Studies Sheet to CvT (after adding fk_extraction_document_id from idList)
  #####################################################################################
  doc_sheet_list$Studies$fk_extraction_document_id = doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$document_type == 1]
  
  # If multiple documents (reference docs)
  if(nrow(doc_sheet_list$Documents[doc_sheet_list$Documents$document_type == 2,])){
    stop("NEED TO FLESH OUT REF DOC ID MATCHING NAMES OUTPUT")
    #Join by id values matching to documents sheet of template
    # doc_sheet_list$Studies = doc_sheet_list$Studies %>%
    #   dplyr::rename(fk_doc_id = fk_reference_document_id) %>%
    #   left_join(doc_sheet_list$Documents %>%
    #               filter(document_type == 2) %>%
    #               select(id, fk_reference_document_id=fk_document_id) %>%
    #               mutate(id = as.numeric(id)),
    #             by=c("fk_doc_id" = "id"))  
    
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::rename(fk_doc_id = fk_reference_document_id) %>%
      dplyr::left_join(doc_sheet_list$Documents %>% 
                         dplyr::select(id, fk_reference_document_id=fk_document_id),
                by=c("fk_doc_id"="id"))
    
  } else {#No reference documents to match
    doc_sheet_list$Studies$fk_reference_document_id = as.numeric(NA)
  }
  
  # Match test chemicals to chemicals table entries based on mapped DSSTox information
  cvt_chemicals = db_query_cvt("SELECT id As fk_dosed_chemical_id, dsstox_substance_id, dsstox_casrn, preferred_name FROM cvt.chemicals")
  doc_sheet_list$Studies = doc_sheet_list$Studies %>%
    dplyr::left_join(cvt_chemicals, by=c("dsstox_substance_id", "dsstox_casrn", "preferred_name"))
  
  if(map_chemicals & any(is.na(doc_sheet_list$Studies$fk_dosed_chemical_id))){
    stop("...Found test chemical entries without chemicals table entry...")
  }
  
  ### Administration Route Matching ###
  match_check = nrow(doc_sheet_list$Studies)
  
  doc_sheet_list$Studies = doc_sheet_list$Studies %>%
    # Remove if already present, matching here now
    dplyr::select(-fk_administration_route, -administration_route_normalized) %>% 
    dplyr::left_join(db_query_cvt("SELECT * FROM cvt.administration_route_dict") %>%
                       dplyr::select(fk_administration_route = id, administration_route_original, administration_route_normalized),
              by=c("administration_route_original"))
  
  if(match_check != nrow(doc_sheet_list$Studies)){
    stop("...Administration route matching produced duplicates...check...")
  }
  
  message("...pushing to Studies table")
  # Get Studies table fields
  tbl_fields = db_query_cvt("SELECT * FROM cvt.studies limit 1") %>% 
    names() %>%
    .[!. %in% col_exclude]
  # names(doc_sheet_list$Studies)[!names(doc_sheet_list$Studies) %in% tbl_fields]
  browser()
  db_push_to_CvT(df=doc_sheet_list$Studies %>%
                   dplyr::select(dplyr::any_of(tbl_fields)),
              tblName="studies")
  
  # Get Studies ID values (Assumes the query returns the rows in the same order they were uploaded...)
  match_entry = db_query_cvt(paste0("SELECT id as fk_studies_id FROM cvt.studies ORDER BY id DESC LIMIT ", 
                                    nrow(doc_sheet_list$Studies))) %>%
    dplyr::arrange(fk_studies_id)
  
  doc_sheet_list$Studies = cbind(doc_sheet_list$Studies, match_entry)
  
  #####################################################################################
  #### Push Subjects Sheet to CvT (no fk to add)
  #####################################################################################
  message("...pushing to Subjects table")
  # Get Subjects table fields
  tbl_fields = db_query_cvt("SELECT * FROM cvt.subjects limit 1") %>% 
    names() %>%
    .[!. %in% col_exclude]
  # names(doc_sheet_list$Subjects)[!names(doc_sheet_list$Subjects) %in% tbl_fields]
  browser()
  db_push_to_CvT(df=doc_sheet_list$Subjects %>%
                   dplyr::select(dplyr::any_of(tbl_fields)),
                 tblName="subjects")
  
  # Get Subjects ID Values (Assumes the query returns the rows in the same order they were uploaded...)
  match_entry = db_query_cvt(paste0("SELECT id as fk_subjects_id FROM cvt.subjects ORDER BY id DESC LIMIT ", 
                                    nrow(doc_sheet_list$Subjects))) %>%
    dplyr::arrange(fk_subjects_id)
  
  doc_sheet_list$Subjects = cbind(doc_sheet_list$Subjects, match_entry)
  
  ##############################################################################
  #### Push Series Sheet to CvT (matching to fk_study_id and fk_subject_id)
  ##############################################################################
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    left_join(doc_sheet_list$Studies %>% #Left join so it only joins what records match
                select(fk_study_id=id, new_fk_study_id=fk_studies_id) %>%
                mutate(fk_study_id = as.numeric(fk_study_id)),
              by="fk_study_id") %>%
    left_join(doc_sheet_list$Subjects %>% #Left join so it only joins what records match
                select(fk_subject_id=id, new_fk_subject_id=fk_subjects_id) %>%
                mutate(fk_subject_id = as.numeric(fk_subject_id)),
              by="fk_subject_id") %>%
    mutate(fk_study_id = new_fk_study_id,
           fk_subject_id = new_fk_subject_id) %>%
    select(-new_fk_subject_id, -new_fk_study_id)
  
  # Match analyte chemicals to chemicals table entries based on mapped DSSTox information
  cvt_chemicals = db_query_cvt("SELECT id As fk_analyzed_chemical_id, dsstox_substance_id, dsstox_casrn, preferred_name FROM cvt.chemicals")
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    left_join(cvt_chemicals, by=c("dsstox_substance_id", "dsstox_casrn", "preferred_name"))
  
  if(map_chemicals & any(is.na(doc_sheet_list$Series$fk_analyzed_chemical_id))){
    stop("...Found analyte chemical entries without chemicals table entry...")
  }
  
  ### Conc_Medium Matching ###
  match_check = nrow(doc_sheet_list$Series)
  
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    dplyr::select(-conc_medium, -conc_medium_normalized, -conc_medium_id) %>% #remove if already present, matching here now
    dplyr::left_join(db_query_cvt("SELECT * FROM cvt.conc_medium_dict") %>%
                dplyr::select(fk_conc_medium_id = id, conc_medium_original),
              by=c("conc_medium_original"))
  
  if(match_check != nrow(doc_sheet_list$Series)){
    stop("...Concentration Medium matching produced duplicates...check")
  }
  
  message("...pushing to Series table")
  # Get Series table fields
  tbl_fields = db_query_cvt("SELECT * FROM cvt.series limit 1") %>% 
    names() %>%
    .[!. %in% col_exclude]
  # names(doc_sheet_list$Series)[!names(doc_sheet_list$Series) %in% tbl_fields]
  browser()
  db_push_to_CvT(df=doc_sheet_list$Series %>%
                   dplyr::select(dplyr::any_of(tbl_fields)),
                 tblName="series")
  
  # Get Series ID Values (Assumes the query returns the rows in the same order they were uploaded...)
  match_entry = db_query_cvt(paste0("SELECT id as new_fk_series_id FROM cvt.series ORDER BY id DESC LIMIT ", 
                                    nrow(doc_sheet_list$Series))) %>%
    dplyr::arrange(new_fk_series_id)
  
  doc_sheet_list$Series = cbind(doc_sheet_list$Series, match_entry)
  
  ##############################################################################
  #### Push Conc_Time_Values Sheet to CvT (matching to fk_series_id
  ##############################################################################
  doc_sheet_list$Conc_Time_Values = doc_sheet_list$Conc_Time_Values %>%
    dplyr::left_join(doc_sheet_list$Series %>% 
                       dplyr::select(fk_series_id=id, new_fk_series_id), by=c("fk_series_id")) %>%
    dplyr::mutate(fk_series_id = new_fk_series_id) %>%
    dplyr::select(-new_fk_series_id, -id, -species)
  
  message("...pushing to Conc_Time_Values table")
  # Get Conc_Time_Values table fields
  tbl_fields = db_query_cvt("SELECT * FROM cvt.conc_time_values limit 1") %>% 
    names() %>%
    .[!. %in% col_exclude]
  # names(doc_sheet_list$Conc_Time_Values)[!names(doc_sheet_list$Conc_Time_Values) %in% tbl_fields]
  browser()
  db_push_to_CvT(df=doc_sheet_list$Conc_Time_Values %>%
                   dplyr::select(dplyr::any_of(tbl_fields)),
                 tblName="conc_time_values")
  
  # Write export file  
  writexl::write_xlsx(doc_sheet_list, path=paste0("output/", 
                                                  basename(c_files_list$filename[i]) %>% gsub(".xlsx", "", .), 
                                                  "_loaded_", format(Sys.time(), "%Y%m%d"), 
                                                  ".xlsx"))
}
message("Done...", Sys.time())

