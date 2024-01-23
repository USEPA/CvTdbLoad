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
    query = paste0("SELECT id FROM cvt.documents WHERE curation_set_tag = '", cvt_dataset,"'")
  } else {
    query = "SELECT id FROM cvt.documents"
  }
  
  fileList = db_query_cvt(query)
  
  for(i in seq_len(nrow(fileList))){
    f = fileList$id[i]
    message("Normalizing file (", i, "/", nrow(fileList),"): ", f, "...", Sys.time())
    
    doc_sheet_list = cvtdb_to_template(id=list(id=f),
                                       template_path="input/CvT_data_template_articles.xlsx", 
                                       template_map="input/qa_template_map.xlsx",
                                       include_foreign_keys=TRUE)
    
    # Create/clear log entry for filename
    log_CvT_doc_load(f=f, 
                     m=NULL, 
                     reset=TRUE,
                     log_path=log_path)
    
    # Pull administration_route_normalized needed for dose normalization
    admin_route_list = db_query_cvt(paste0("SELECT id as fk_administration_route_id, administration_route_normalized FROM cvt.administration_route_dict WHERE ",
                                           "id in (", 
                                           toString(unique(doc_sheet_list$Studies$fk_administration_route_id)), 
                                           ")"))
    
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::left_join(admin_route_list,
                       by="fk_administration_route_id")
    
    # Normalize species
    doc_sheet_list$Subjects$species = normalize_species(x=doc_sheet_list$Subjects$species,
                                                        log_path=log_path)
    
    # Call to the orchestration function for data normalization (with error logging)
    doc_sheet_list = normalize_CvT_data(df=doc_sheet_list, 
                                        f=f,
                                        log_path=log_path)
    
    # Rename columns
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::rename(test_substance_name_original = test_substance_name,
                    test_substance_name_secondary_original = test_substance_name_secondary,
                    test_substance_casrn_original = test_substance_casrn,
                    dose_level_original = dose_level,
                    dose_level_units_original = dose_level_units)
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      dplyr::rename(analyte_name_original = analyte_name,
                    analyte_name_secondary_original = analyte_name_secondary,
                    analyte_casrn_original = analyte_casrn,
                    time_units_original = time_units,
                    conc_units_original=conc_units)
    
    # Append qc_flags from logged flags
    qc_flags = readxl::read_xlsx(log_path) %>%
      dplyr::filter(filename == f) %>%
      dplyr::mutate(across(everything(), ~as.character(.))) %>%
      tidyr::pivot_longer(cols=-c(filename, timestamp),
                          names_to = "flag") %>%
      dplyr::filter(value == 1) %>%
      dplyr::pull(flag) %>%
      toString()
    
    doc_sheet_list$Documents$qc_flags = qc_flags
    
    # Push update
    for(s in names(doc_sheet_list)){
      message("...Pushing ", s," sheet updates...")
      # Get documents table fields
      tbl_fields = db_query_cvt(paste0("SELECT * FROM cvt.",s," limit 1")) %>% 
        names()
      # tbl_fields[!tbl_fields %in% names(doc_sheet_list[[s]])]
      
      data_in_db = doc_sheet_list[[s]] %>%
        # Order columns by database table order
        dplyr::select(any_of(tbl_fields)) %>%
        dplyr::select(-starts_with("fk_")) %>%
        dplyr::select(-ends_with("_original"))
      
      # Update database entry for document
      db_update_tbl(df=data_in_db,
                    # %>% 
                    #   dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.))),
                    tblName = s)
    }
    
    
  }
}
