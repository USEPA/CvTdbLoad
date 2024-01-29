# Script to push CvT Original Extracted Values to CvT from Clowder
# This decouples the normalization from loading original values first.
# Created by: Jonathan Taylor Wall
# Created Date: 2023-12-14

tmp_load_cvt <- function(){
  # Exclude versioning fields
  col_exclude = c("id", "created_by", "rec_create_dt", "qc_status", "qc_flags", "qc_notes", "version")
  
  apiKey = Sys.getenv("apiKey")
  baseurl = Sys.getenv("baseurl")
  dsID = Sys.getenv("file_dsID")
  doc_dsID = Sys.getenv("doc_dsID")
  cvt_dataset = "PCB"
  schema = "cvt"
  log_path = "output/load_required_fields_log.xlsx"
  
  # Query already loaded Jira tickets
  loaded_jira_docs = db_query_cvt("SELECT jira_ticket FROM cvt.documents WHERE jira_ticket IS NOT NULL")
  # Pull dataset ticket templates and filter to those not loaded
  to_load = pull_clowder_files_to_load(dsID, baseurl, apiKey, curation_set_tag=cvt_dataset) %>%
    dplyr::filter(!jira_ticket %in% loaded_jira_docs$jira_ticket)
  
  # Only process if Clowder File records pulled
  if(nrow(to_load)){
    
    # Loop through Clowder files to load
    for(i in seq_len(nrow(to_load))){
      message("Pushing file (", i, "/", nrow(to_load),"): ", toString(to_load[i,c("jira_ticket", "filename")]), "...", Sys.time())
      # Boolean of whether to only load document sheet
      load_doc_sheet_only = FALSE
      update_doc_in_db = FALSE
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
        # Select log columns with value of -1
        # https://stackoverflow.com/questions/63743572/select-columns-based-on-column-value-range-with-dplyr
        dplyr::select(where(~any(. == -1)))
      # Warn user of requirements issues with file
      if(length(req_fields_check)){
        message("File missing required fields: ")
        cat(paste0("- ", names(req_fields_check)), sep="\n")
        # browser()
        next
      }
      # stop("Found passing file to load!")
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
      # Ignore reference documents already matched from database
      if(any(!is.na(doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$document_type == 1]))){
        message("...Document entry already in CvTdb...")
        # Check if any study data is associated with document
        doc_studies = db_query_cvt(paste0("SELECT * FROM cvt.studies where fk_extraction_document_id = ", 
                                          doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$document_type == 1]))
        # Log case of duplicate document with studies
        if(nrow(doc_studies)){
          message("Skipping duplicate document already in database with associated studies...")
          log_CvT_doc_load(f=f, 
                           m="duplicate_doc_in_db_with_studies", 
                           log_path=log_path)
          next
        }
        # Set boolean to update doc db information
        update_doc_in_db = TRUE
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
      # If document already present, but without associations, remove old record and append new
      if(update_doc_in_db){
        # Get documents table fields
        tbl_fields = db_query_cvt("SELECT * FROM cvt.documents limit 1") %>% 
          names()
        doc_in_db = db_query_cvt(paste0("SELECT * FROM cvt.documents where id = ",
                                        doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$document_type == 1]))
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
          dplyr::select(-rec_create_dt, -version, -created_by) %>%
          # Order columns by database table order
          dplyr::select(any_of(tbl_fields), document_type)
        
        # Update database entry for document
        db_update_tbl(df=doc_in_db %>%
                        dplyr::select(-document_type),
                      tblName = "documents")
        
        # Match back new document records
        doc_sheet_list$Documents = doc_in_db %>%
          dplyr::mutate(fk_document_id = id)
        
      } else {
        message("...pushing to Documents table")
        # Get documents table fields
        tbl_fields = db_query_cvt("SELECT * FROM cvt.documents limit 1") %>% 
          names() %>%
          .[!. %in% col_exclude]
        # names(doc_sheet_list$Documents)[!names(doc_sheet_list$Documents) %in% tbl_fields]
        browser()
        db_push_rs = db_push_tbl_to_db(dat=doc_sheet_list$Documents %>%
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
        doc_sheet_list$Documents = match_cvt_doc_to_db_doc(df = doc_sheet_list$Documents %>%
                                                             dplyr::select(-fk_document_id))
      }
      
      # Check if any did not match to previously pushed document entry
      if(any(is.na(doc_sheet_list$Documents$fk_document_id))){
        message("Error mapping to pushed document entry...")
        browser()
      }
      
      # Push Document Lineage Linkages
      doc_lineage = doc_sheet_list$Documents %>%
        dplyr::select(fk_doc_id = fk_document_id, 
                      relationship_type = document_type)
      # Add parent doc (document_type == 1)
      doc_lineage$fk_parent_doc_id = doc_lineage$fk_doc_id[doc_lineage$relationship_type == 1]
      doc_lineage = doc_lineage %>%
        dplyr::filter(fk_parent_doc_id != fk_doc_id) %>%
        dplyr::mutate(relationship_type = dplyr::case_when(
          relationship_type == 2 ~ "Reference Document",
          TRUE ~ as.character(relationship_type)
        ))
      
      message("...pushing to Document Lineage table")
      browser()
      db_push_tbl_to_db(dat=doc_lineage,
                        tblName="documents_lineage",
                        overwrite=FALSE, 
                        append=TRUE)
      
      #####################################################################################
      #### Push Studies Sheet to CvT (after adding fk_extraction_document_id from idList)
      #####################################################################################
      doc_sheet_list$Studies$fk_extraction_document_id = doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$document_type == 1]
      
      # If multiple documents (reference docs)
      if(nrow(doc_sheet_list$Documents[doc_sheet_list$Documents$document_type != 1,])){
        # stop("NEED TO FLESH OUT REF DOC ID MATCHING NAMES OUTPUT")
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
      db_push_rs = db_push_tbl_to_db(dat=doc_sheet_list$Studies %>%
                                       dplyr::select(dplyr::any_of(tbl_fields)),
                                     tblName="studies",
                                     overwrite=FALSE, 
                                     append=TRUE)
      if(is.null(db_push_rs) || is.na(db_push_rs)){
        message("Issue pushing studies table.")
        browser()
      }
      
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
      db_push_rs = db_push_tbl_to_db(dat=doc_sheet_list$Subjects %>%
                          dplyr::select(dplyr::any_of(tbl_fields)),
                        tblName="subjects",
                        overwrite=FALSE, 
                        append=TRUE)
      if(is.null(db_push_rs) || is.na(db_push_rs)){
        message("Issue pushing subjects table.")
        browser()
      }
      
      # Get Subjects ID Values (Assumes the query returns the rows in the same order they were uploaded...)
      match_entry = db_query_cvt(paste0("SELECT id as fk_subjects_id FROM cvt.subjects ORDER BY id DESC LIMIT ", 
                                        nrow(doc_sheet_list$Subjects))) %>%
        dplyr::arrange(fk_subjects_id)
      
      doc_sheet_list$Subjects = cbind(doc_sheet_list$Subjects, match_entry)
      
      ##############################################################################
      #### Push Series Sheet to CvT (matching to fk_study_id and fk_subject_id)
      ##############################################################################
      doc_sheet_list$Series = doc_sheet_list$Series %>%
        dplyr::mutate(fk_study_id = as.numeric(fk_study_id),
                      fk_subject_id = as.numeric(fk_subject_id)) %>%
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
      
      # Check if foreign key matching was successful
      if(anyNA(doc_sheet_list$Series$fk_study_id)){
        message("Unmapped Series fk_study_id")
        browser()
        stop("Unmapped Series fk_study_id")
      }
      
      if(anyNA(doc_sheet_list$Series$fk_subject_id)){
        message("Unmapped Series fk_subject_id")
        browser()
        stop("Unmapped Series fk_subject_id")
      }
      
      message("...pushing to Series table")
      # Get Series table fields
      tbl_fields = db_query_cvt("SELECT * FROM cvt.series limit 1") %>% 
        names() %>%
        .[!. %in% col_exclude]
      # names(doc_sheet_list$Series)[!names(doc_sheet_list$Series) %in% tbl_fields]
      browser()
      db_push_rs = db_push_tbl_to_db(dat=doc_sheet_list$Series %>%
                          dplyr::select(dplyr::any_of(tbl_fields)),
                        tblName="series",
                        overwrite=FALSE, 
                        append=TRUE)
      
      if(is.null(db_push_rs) || is.na(db_push_rs)){
        message("Issue pushing series table.")
        browser()
      }
      
      # Get Series ID Values (Assumes the query returns the rows in the same order they were uploaded...)
      match_entry = db_query_cvt(paste0("SELECT id as new_fk_series_id FROM cvt.series ORDER BY id DESC LIMIT ", 
                                        nrow(doc_sheet_list$Series))) %>%
        dplyr::arrange(new_fk_series_id)
      
      doc_sheet_list$Series = cbind(doc_sheet_list$Series, match_entry)
      
      ##############################################################################
      #### Push Conc_Time_Values Sheet to CvT (matching to fk_series_id
      ##############################################################################
      doc_sheet_list$Conc_Time_Values = doc_sheet_list$Conc_Time_Values %>%
        dplyr::mutate(fk_series_id = as.numeric(fk_series_id)) %>%
        dplyr::left_join(doc_sheet_list$Series %>% 
                           dplyr::select(fk_series_id=id, new_fk_series_id), by=c("fk_series_id")) %>%
        dplyr::mutate(fk_series_id = new_fk_series_id) %>%
        dplyr::select(-new_fk_series_id)
      
      # Check if foreign key matching was successful
      if(anyNA(doc_sheet_list$Conc_Time_Values$fk_series_id)){
        message("Unmapped Conc_Time_Values fk_series_id")
        browser()
        stop("Unmapped Conc_Time_Values fk_series_id")
      }
      
      message("...pushing to Conc_Time_Values table")
      # Get Conc_Time_Values table fields
      tbl_fields = db_query_cvt("SELECT * FROM cvt.conc_time_values limit 1") %>% 
        names() %>%
        .[!. %in% col_exclude]
      # names(doc_sheet_list$Conc_Time_Values)[!names(doc_sheet_list$Conc_Time_Values) %in% tbl_fields]
      browser()
      db_push_rs = db_push_tbl_to_db(dat=doc_sheet_list$Conc_Time_Values %>%
                                       dplyr::select(dplyr::any_of(tbl_fields)),
                                     tblName="conc_time_values",
                                     overwrite=FALSE, 
                                     append=TRUE)
      if(is.null(db_push_rs) || is.na(db_push_rs)){
        message("Issue pushing conc_time_values table.")
        browser()
      }
      message("Exporting log")
      browser()
      # Export loaded template log
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
}

