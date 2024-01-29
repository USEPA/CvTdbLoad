# Script to apply normalization logic to CvT database records
# This decouples the normalization from loading original values first.
# Created by: Jonathan Taylor Wall
# Created Date: 2024-1-23

# TODO Eventually update to not have to source functions directly
source("scripts/QA/cvtdb_to_template.R")

normalize_CvT_db <- function(){
  apiKey = Sys.getenv("apiKey")
  baseurl = Sys.getenv("baseurl")
  dsID = Sys.getenv("file_dsID")
  doc_dsID = Sys.getenv("doc_dsID")
  cvt_dataset = "PCB"
  schema = "cvt"
  log_path = "output/db_normalize_log.xlsx"
  
  # Query templates to normalize
  if(!is.null(cvt_dataset)){
    query = paste0("SELECT id FROM cvt.documents WHERE curation_set_tag = '", 
                   # Temporary filtering to those not already normalized
                   cvt_dataset,
                   "' AND version = 1",
                   " ORDER BY id")
  } else {
    query = "SELECT id FROM cvt.documents order by id"
  }
  
  fileList = db_query_cvt(query)
  
  for(i in seq_len(nrow(fileList))){
    f = fileList$id[i]
    message("Normalizing file (", i, "/", nrow(fileList),"): ", f, "...", Sys.time())
    
    doc_sheet_list = get_cvt_by_doc_id(id=list(id=f))
    
    # Create/clear log entry for filename
    log_CvT_doc_load(f=f, 
                     m=NULL, 
                     reset=TRUE,
                     log_path=log_path)
    
    # Pull administration_route_normalized needed for dose normalization
    admin_route_list = db_query_cvt(paste0(
      "SELECT id as fk_administration_route_id, administration_route_original, administration_route_normalized ",
      "FROM cvt.administration_route_dict WHERE ",
      "id in (", 
      toString(unique(doc_sheet_list$Studies$fk_administration_route_id)), 
      ")")
    )
    # Fill in administration original and normalized
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::select(-administration_route_original, -administration_route_normalized) %>%
      dplyr::left_join(admin_route_list,
                       by="fk_administration_route_id")
    
    # Pull conc_medium_normalized needed for conc normalization
    conc_medium_list = db_query_cvt(paste0(
      "SELECT id as fk_conc_medium_id, conc_medium_original, conc_medium_normalized ",
      "FROM cvt.conc_medium_dict WHERE ",
      "id in (", 
      toString(unique(doc_sheet_list$Series$fk_conc_medium_id)), 
      ")")
    )
    # Fill in administration original and normalized
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      dplyr::select(-conc_medium_original, -conc_medium_normalized) %>%
      dplyr::left_join(conc_medium_list,
                       by="fk_conc_medium_id")
    
    # Normalize species
    doc_sheet_list$Subjects = normalize_species(x=doc_sheet_list$Subjects,
                                                        log_path=log_path)
    
    # Call to the orchestration function for data normalization (with error logging)
    doc_sheet_list = normalize_CvT_data(df=doc_sheet_list, 
                                        f=f,
                                        log_path=log_path)
    
    
    # Append qc_flags from logged flags
    flag_map = readxl::read_xlsx("input/dictionaries/flag_map.xlsx") %>%
      dplyr::select(-Definition, -`Flag Type`)
    
    norm_qc_flags = readxl::read_xlsx(log_path) %>%
      dplyr::filter(filename == f) %>%
      dplyr::mutate(across(everything(), ~as.character(.))) %>%
      tidyr::pivot_longer(cols=-c(filename, timestamp),
                          names_to = "qc_flags_new") %>%
      dplyr::filter(value != 0) %>%
      dplyr::select(qc_flags_new, index=value) %>%
      dplyr::left_join(flag_map,
                       by=c("qc_flags_new"="Field Name")) %>%
      tidyr::separate_rows(index, sep=",") %>%
      dplyr::mutate(index = as.numeric(index)) %>%
      dplyr::filter(!is.na(index)) %>%
      dplyr::distinct()
    
    if(any(is.na(norm_qc_flags$sheet))){
      stop("Need to curate qc_flags: ", toString(
        unique(
          norm_qc_flags$qc_flags_new[is.na(norm_qc_flags$sheet)]
          )
        )
      )
    }
    
    # Combine indexes with multiple QC flags
    norm_qc_flags = norm_qc_flags %>%
      dplyr::group_by(sheet, index) %>%
      dplyr::mutate(qc_flags_new = paste0(qc_flags_new, collapse = ", ")) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()
    
    # Append qc_flags per sheet
    for(s in unique(norm_qc_flags$sheet)){
      # doc_sheet_list[[s]]$qc_flags = NA
      
      # Add flags by logged record ID, ensure unique flag list
      doc_sheet_list[[s]] = doc_sheet_list[[s]] %>%
        dplyr::left_join(norm_qc_flags %>%
                           dplyr::select(qc_flags_new, index),
                         by=c("id"='index')) %>%
        tidyr::unite(col="qc_flags", qc_flags, qc_flags_new, sep = ", ", na.rm = TRUE) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(qc_flags = toString(unique(unlist(strsplit(qc_flags,",\\s+"))))) %>%
        dplyr::ungroup()
      # Convert empty flags back to NA
      doc_sheet_list[[s]]$qc_flags[doc_sheet_list[[s]]$qc_flags %in% c("")] <- NA
    }
    
    # Push update
    for(s in names(doc_sheet_list)){
      message("...Pushing ", s," sheet updates...")
      # Get documents table fields
      tbl_fields = db_query_cvt(paste0("SELECT * FROM cvt.", s," limit 1")) %>% 
        names()
      # tbl_fields[!tbl_fields %in% names(doc_sheet_list[[s]])]
      
      data_in_db = doc_sheet_list[[s]] %>%
        # Order columns by database table order
        dplyr::select(any_of(tbl_fields))
      
      # Update database entry for document
      db_update_tbl(df=data_in_db,
                    # %>% 
                    #   dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.))),
                    tblName = s)
    }
    message("Continue?")
    browser()
  }
}
