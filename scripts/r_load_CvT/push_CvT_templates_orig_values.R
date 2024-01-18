# Script to push CvT Original Extracted Values to CvT from Clowder
# This decouples the normalization from loading original values first.
# Created by: Jonathan Taylor Wall
# Created Date: 2023-12-14

# Exclude versioning fields
col_exclude = c("id", "created_by", "rec_create_dt", "qc_status", "qc_flags", "qc_notes", "version")

apiKey = Sys.getenv("apiKey")
baseurl = Sys.getenv("baseurl")
dsID = Sys.getenv("file_dsID")
doc_dsID = Sys.getenv("doc_dsID")
cvt_dataset = "PCB"
schema = "cvt"
log_path = "output/load_required_fields_log.xlsx"

to_load = pull_clowder_files_to_load(dsID, baseurl, apiKey, curation_set_tag=cvt_dataset)

# Only process if Clowder File records pulled
if(nrow(to_load)){
  
  # Loop through Clowder files to load
  for(i in seq_along(nrow(to_load))){
    message("Pushing file (", i, "/", nrow(to_load),"): ", toString(to_load[i,c("jira_ticket", "filename")]), "...", Sys.time())
    # Boolean of whether to only load document sheet
    load_doc_sheet_only = FALSE
    # Filename
    f = to_load$filename[i]
    # Create/clear log entry for filename
    log_CvT_doc_load(f, m=NULL, reset=TRUE, 
                     log_path = log_path)
    # Pull temp file to process
    doc_sheet_list = load_file_from_api(url = paste0(baseurl,"/api/files/",to_load$clowder_id[i],"/blob"),
                                        headers = c(`X-API-Key` = apiKey),
                                        mode = "wb",
                                        file_type = "xlsx")
    
    # Check for template with only Documents sheet
    if(length(doc_sheet_list) == 1 & all(names(doc_sheet_list) == "Documents")){
      load_doc_sheet_only = TRUE
    }
    # Check for extracted field values - only 1-3 are loading data
    if(any(!doc_sheet_list$Documents$extracted %in% 1:3)){
      load_doc_sheet_only = TRUE
    }
    
    # Required field validation check
    check_required_fields_validator(df = doc_sheet_list, 
                                    f = f,
                                    log_path = log_path)
    
    req_fields_check = readxl::read_xlsx(log_path) %>%
      dplyr::filter(filename == f) %>%
      # Select log columns with value of 1
      # https://stackoverflow.com/questions/63743572/select-columns-based-on-column-value-range-with-dplyr
      dplyr::select(where(~any(. == 1)))
    # Warn user of requirements issues with file
    if(length(req_fields_check)){
      message("File missing required fields: ")
      cat(paste0("- ", names(req_fields_check)), sep="\n")
      browser()
      next
    }
    
    # Rename "original" fields
    doc_sheet_list = set_original_fields(sheet_list=doc_sheet_list, schema = schema)
    # Update database dictionaries and get dictionary foreign keys    
    doc_sheet_list = get_dict_update_ids(sheet_list=doc_sheet_list, schema = schema)
    
    # Rename foreign key fields as needed
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::rename(fk_dosed_chemical_id=fk_chemicals_id)
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      dplyr::rename(fk_analyzed_chemical_id=fk_chemicals_id)
    
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
      dplyr::mutate(jira_ticket = to_load$jira_ticket[i],
                    curation_set_tag = to_load$curation_set_tag[i],
                    clowder_template_id = to_load$clowder_id[i])
    # TODO - Improve Clowder ID mapping logic (case where template has clowder_id field)
    # Match to Clowder documents
    doc_sheet_list$Documents = clowder_match_docs(df=doc_sheet_list$Documents,
                                                  dsID=doc_dsID,
                                                  baseurl=baseurl,
                                                  apiKey=apiKey)
    
    message("...pushing to Documents table")
    # Get documents table fields
    tbl_fields = db_query_cvt("SELECT * FROM cvt.documents limit 1") %>% 
      names() %>%
      .[!. %in% col_exclude]
    # names(doc_sheet_list$Documents)[!names(doc_sheet_list$Documents) %in% tbl_fields]
    browser()
    db_push_tbl_to_db(dat=doc_sheet_list$Documents %>%
                        dplyr::select(dplyr::any_of(tbl_fields)),
                      tblName="documents",
                      overwrite=FALSE, 
                      append=TRUE)
    
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
  
    message("...pushing to Studies table")
    # Get Studies table fields
    tbl_fields = db_query_cvt("SELECT * FROM cvt.studies limit 1") %>% 
      names() %>%
      .[!. %in% col_exclude]
    # names(doc_sheet_list$Studies)[!names(doc_sheet_list$Studies) %in% tbl_fields]
    browser()
    db_push_tbl_to_db(dat=doc_sheet_list$Studies %>%
                        dplyr::select(dplyr::any_of(tbl_fields)),
                      tblName="studies",
                      overwrite=FALSE, 
                      append=TRUE)
    
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
    db_push_tbl_to_db(dat=doc_sheet_list$Subjects %>%
                        dplyr::select(dplyr::any_of(tbl_fields)),
                      tblName="subjects",
                      overwrite=FALSE, 
                      append=TRUE)
    
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
    
    message("...pushing to Series table")
    # Get Series table fields
    tbl_fields = db_query_cvt("SELECT * FROM cvt.series limit 1") %>% 
      names() %>%
      .[!. %in% col_exclude]
    # names(doc_sheet_list$Series)[!names(doc_sheet_list$Series) %in% tbl_fields]
    browser()
    db_push_tbl_to_db(dat=doc_sheet_list$Series %>%
                        dplyr::select(dplyr::any_of(tbl_fields)),
                      tblName="series",
                      overwrite=FALSE, 
                      append=TRUE)
    
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
      dplyr::select(-new_fk_series_id)
    
    message("...pushing to Conc_Time_Values table")
    # Get Conc_Time_Values table fields
    tbl_fields = db_query_cvt("SELECT * FROM cvt.conc_time_values limit 1") %>% 
      names() %>%
      .[!. %in% col_exclude]
    # names(doc_sheet_list$Conc_Time_Values)[!names(doc_sheet_list$Conc_Time_Values) %in% tbl_fields]
    browser()
    db_push_tbl_to_db(dat=doc_sheet_list$Conc_Time_Values %>%
                        dplyr::select(dplyr::any_of(tbl_fields)),
                      tblName="conc_time_values",
                      overwrite=FALSE, 
                      append=TRUE)
    
    output_dir = file.path("output", "Document Loading", cvt_dataset)
    if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    # Write export file  
    writexl::write_xlsx(doc_sheet_list, path=paste0(output_dir,"/", 
                                                    basename(to_load$filename[i]) %>% gsub(".xlsx", "", .), 
                                                    "_loaded_", format(Sys.time(), "%Y%m%d"), 
                                                    ".xlsx"))
    
  }
}

message("Done. - ", Sys.time())
