# Script to push CvT Original Extracted Values to CvT from Clowder
# This decouples the normalization from loading original values first.
# Created by: Jonathan Taylor Wall
# Created Date: 2023-12-14

# Exclude versioning fields
col_exclude = c("created_by", "rec_create_dt", "qc_status", "qc_flags", "qc_notes", "version")

apiKey = Sys.getenv("apiKey")
baseurl = Sys.getenv("baseurl")
dsID = Sys.getenv("file_dsID")
doc_dsID = Sys.getenv("doc_dsID")
cvt_dataset = "PFAS_PIP"
schema = "cvt"

to_load = pull_clowder_files_to_load(dsID, baseurl, apiKey, curation_set_tag=cvt_dataset)

# Only process if Clowder File records pulled
if(nrow(to_load)){
  
  # Loop through Clowder files to load
  for(i in seq_along(nrow(to_load))){
    message("Pushing file (", i, "/", nrow(to_load),"): ", toString(to_load[i,c("jira_ticket", "filename")]), "...", Sys.time())
    # Pull temp file to process
    doc_sheet_list = load_file_from_api(url = paste0(baseurl,"/api/files/",to_load$clowder_id[i],"/blob"),
                                        headers = c(`X-API-Key` = apiKey),
                                        mode = "wb",
                                        file_type = "xlsx")
    
    # Rename "original" fields
    doc_sheet_list = set_original_fields(sheet_list=doc_sheet_list, schema = schema)
    # Update database dictionaries and get dictionary foreign keys    
    doc_sheet_list = get_dict_update_ids(sheet_list=doc_sheet_list, schema = schema)

    # Push chemical entries to chemicals table, add clean name/casrn
    
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
  
      
    
  }
}

message("Done. - ", Sys.time())
