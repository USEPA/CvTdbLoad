#Script to push CvT extracted docs to CvT database
#Created by: Jonathan Taylor Wall
#Created Date: 2021-02-25
#Load packages
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
invisible(sapply(file_source, source,.GlobalEnv))
################################################################################
###Main Script Section
################################################################################
#outputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA/normalized_templates"
outputDir = "output/normalized_templates"#"L:/Lab/HEM/T_Wall_Projects_FY20/CvT Database/output/normalized_templates"
template_path = "output/CvT_normalized_template.xlsx"#L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/CvT_data_template_articles.xlsx"
sheetList = c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values")
normalization_log = readxl::read_xlsx("output/template_metadata_qa.xlsx")
###########################
#Pull directly from Clowder
###########################
#push_doc_list = get_clowder_docList(apiKey, clowder_dataset)
#download_clowder_docs(docData = push_doc_list, outputDir = outputDir, apiKey = apiKey)

###########################
#Push to CvT
###########################
#Push 1 document at a time because of the need to pull unique ID values auto-generated
#when pushed to a database table

fileList = list.files(outputDir, full.names = TRUE, pattern=".xlsx")
fileList = fileList[!grepl("~", fileList)] #Remove tmp files
#Quick filter to Japan dataset
# fileList = c("L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\toQA\\20210106_SPU_rat_PK-CvT_No1_159chem_CRT2020_0c00009(Kamiya et al)_clc.xlsx",
#              "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\toQA\\20210501_SPU_rat_PK-CvT_No4_77chem_clc.xlsx")

for(i in seq_len(length(fileList))){
  # if(i <= 14){#Quick skip/restart logic
  #   next
  # }
  f = fileList[i]
  ######insert loop over fileList logic########
  message("Pushing file (", i, "/", length(fileList),"): ", f, "...", Sys.time())
  #Load Documents Sheet
  doc_sheet_list = load_sheet_group(fileName = f, template_path = template_path)
  
  #Mutate column types to match database
  doc_sheet_list$Documents = doc_sheet_list$Documents %>%
    mutate(across(c(id, document_type, pmid, year), as.numeric))
  doc_sheet_list$Studies = doc_sheet_list$Studies %>%
    mutate(across(c(fk_reference_document_id, dermal_dose_vehicle_pH,
                    dermal_applied_area, aerosol_particle_diameter_mean,
                    aerosol_particle_diameter_gsd, aerosol_particle_density,
                    fk_administration_route), as.numeric))
  doc_sheet_list$Subjects = doc_sheet_list$Subjects %>%
    mutate(across(ends_with("units"), ~gsub("missing_units", NA, .))) %>%
    mutate(across(c(weight_estimated, weight_kg, height_cm), as.numeric))
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    mutate(across(c(x_min, x_max, y_min, y_max, loq, lod, radiolabeled, 
                    fk_study_id, fk_subject_id, n_subjects_in_series, conc_medium_id), as.numeric))
  doc_sheet_list$Conc_Time_Values = doc_sheet_list$Conc_Time_Values %>%
    mutate(across(c(time_original, time_hr, conc_original, conc_sd_original, conc_lower_bound_original, 
                    conc_upper_bound_original, conc, conc_sd, conc_lower_bound, conc_upper_bound
                    ), as.numeric))
  ###########################################################################
  ###Parse the where clause to search by pmid, other_study_identifier, or doi
  ###########################################################################
  where_clause = list(pmid=paste0(unique(doc_sheet_list$Documents$pmid[!is.na(doc_sheet_list$Documents$pmid)]),
                                  collapse="', '"),
                      other_study_identifier=paste0(unique(doc_sheet_list$Documents$other_study_identifier[!is.na(doc_sheet_list$Documents$other_study_identifier)]),
                                                    collapse="', '"),
                      doi=paste0(unique(doc_sheet_list$Documents$doi[!is.na(doc_sheet_list$Documents$doi)]),
                                 collapse="', '"))
  where_clause = where_clause[lapply(where_clause, stringr::str_length)>0]
  where_clause = lapply(names(where_clause), function(x){
    paste0(x, " in ('", where_clause[[x]], "')")
  }) %>% paste0(., collapse = " OR ")
  
  doc_check = query_cvt(paste0("SELECT id, pmid, other_study_identifier, doi, extracted FROM cvt.documents where ", where_clause))# AND extracted != 0"))
  
  if(nrow(doc_check)){#Need to check for extracted status (loaded but not extracted)
    message("...File already pushed...skipping")
    next
  }
  ###########################################################################
  ###Push Documents Sheet to CvT
  ###########################################################################
  message("...pushing to Documents table")
  push_to_CvT(df=doc_sheet_list$Documents %>%
                select(document_type, pmid, other_study_identifier, doi, 
                       first_author, year, title, url, curator_comment) %>%
                filter(!pmid %in% doc_check$pmid[!is.na(doc_check$pmid)],
                       !other_study_identifier %in% doc_check$other_study_identifier[!is.na(doc_check$other_study_identifier)],
                       !doi %in% doc_check$doi[!is.na(doc_check$doi)]),
              tblName="documents")
  if(match_by_whole_entry){
    stop("manually change query to fit the create date")
    match_entry = query_cvt("select * from cvt.documents where rec_create_dt = '2022-02-01 10:17:52.031871'") %>%
      select(fk_document_id=id, document_type, pmid, other_study_identifier, doi, first_author, year, title, url, curator_comment)
    doc_sheet_list$Documents = doc_sheet_list$Documents %>%
      left_join(match_entry)
    #doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$pmid == 7902233] = 6728
    #doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$pmid == 15704209] = 13346
    #doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$pmid == 28803882] = 23064
  } else {
    #Get Documents ID values - Join back to original data for matching
    #Find by matching pmid, study identifier, or doi

    #Remove NA values
    idFilter = lapply(idFilter, function(x) x[!is.na(x)]) %>% purrr::compact()
    #Join for multiple documents (pmid or other_study_identifier)
    doc_sheet_list$Documents = doc_sheet_list$Documents %>%
      left_join(get_tbl_id(tblName="documents",
                           idFilter = paste0("WHERE ", where_clause)) %>%
                  select(id, pmid, other_study_identifier) %>%
                  rename(fk_document_id = id),
                by=c("pmid", "other_study_identifier"))
  }
  
#####################################################################################
####Push Studies Sheet to CvT (after adding fk_extraction_document_id from idList)
#####################################################################################
  doc_sheet_list$Studies$fk_extraction_document_id = doc_sheet_list$Documents$fk_document_id[doc_sheet_list$Documents$document_type == 1]
  #If multiple documents (reference docs)
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
      left_join(doc_sheet_list$Documents %>% 
                  select(id, fk_reference_document_id=fk_document_id),
                by=c("fk_doc_id"="id"))
    
  } else {#No reference documents to match
    doc_sheet_list$Studies$fk_reference_document_id = as.numeric(NA)
  }
    
  #MATCH TO CHEMICALS TABLE
  if(showa_chemical_match){
    cvt_chemicals = readxl::read_xlsx("/home/jwall01/cvtdb/input/chemicals/PBPKShowaPharma_238mapped_07Dec2021.xlsx") %>%
      select(raw_name=`Query Name`, raw_cas = `Query Casrn`, dsstox_substance_id = `Top HIT DSSTox_Substance_Id`, Validated) %>%
      filter(!is.na(dsstox_substance_id)) %>%
      distinct()
    
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      select(-dsstox_substance_id, -dsstox_casrn, -preferred_name) %>%
      left_join(cvt_chemicals, by=c("test_substance_name_original"="raw_name", 
                                    "test_substance_casrn_original"="raw_cas")) %>%
      select(-Validated)
    
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      left_join(query_cvt("SELECT id as fk_dosed_chemical_id, dsstox_substance_id FROM cvt.chemicals"),
                by="dsstox_substance_id")
    
  } else {
    cvt_chemicals = query_cvt("SELECT id As fk_dosed_chemical_id, dsstox_substance_id, dsstox_casrn, preferred_name FROM cvt.chemicals")
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      left_join(cvt_chemicals, by=c("dsstox_substance_id", "dsstox_casrn", "preferred_name"))
    #Filter to ones that didn't match, add new chemicals table entry
    chem_push = doc_sheet_list$Studies %>%
      filter(is.na(fk_dosed_chemical_id), !is.na(dsstox_substance_id)) %>%
      select(dsstox_substance_id, dsstox_casrn, preferred_name) %>%
      mutate(chemistry_team_mapping = 0) %>%
      distinct()
    if(nrow(chem_push)){
      message("INSERT LOGIC TO ADD NEW CHEMICAL RECORD AND REMATCH fk_dose_chemical_id")
    }
  }
  
  ###Administration Route Matching###
  match_check = nrow(doc_sheet_list$Studies)
  
  doc_sheet_list$Studies = doc_sheet_list$Studies %>%
    select(-fk_administration_route, -administration_route_normalized) %>% #remove if already present, matching here now
    left_join(query_cvt("SELECT * FROM cvt.administration_route_dict") %>%
                select(fk_administration_route = id, administration_route_original, administration_route_normalized),
              by=c("administration_route_original"))
  
  if(match_check != nrow(doc_sheet_list$Studies)){
    stop("...Administration route matching produced duplicates...check")
  }
  message("...pushing to Studies table")
  push_to_CvT(df=doc_sheet_list$Studies %>%
                select(-id, -dsstox_substance_id, -chemistry_team_mapping, -fk_doc_id), 
              tblName="studies")
  # ####Push Subjects Sheet to CvT (no fk to add)
  message("...pushing to Subjects table")
  push_to_CvT(df=doc_sheet_list$Subjects %>%
                select(-age_normalized), tblName="subjects")
  
  if(match_by_whole_entry){
    stop("manually change query to fit the create date")
    match_entry = query_cvt("select * from cvt.studies where rec_create_dt = '2022-02-01 10:31:19.259285'") %>%
      select(-fk_extraction_document_id, -created_by, -updated_by, -rec_update_dt, -rec_create_dt) %>%
      dplyr::rename(fk_studies_id = id)
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      left_join(match_entry)
  } else {
    #Get Studies ID values (Assumes the query returns the rows in the same order they were uploaded...)
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      #mutate(fk_extraction_document_id = as.character(fk_extraction_document_id)) %>%
      left_join(get_tbl_id("studies", #Left join so it only joins what records match
                           idFilter=paste0("WHERE fk_extraction_document_id IN (",
                                           toString(doc_sheet_list$Documents$fk_document_id), ")")) %>%
                  rename(fk_studies_id = id))  
  }
  
  #Get Subjects ID Values (Assumes the query returns the rows in the same order they were uploaded...)
  #Doesn't handle duplicate entries...
  if(match_by_whole_entry){
    stop("manually change query to fit the create date")
    match_entry = query_cvt("select * from cvt.subjects where rec_create_dt = '2022-02-01 10:33:49.959818'") %>%
      select(-notes, -created_by, -updated_by, -rec_update_dt, -rec_create_dt) %>%
      dplyr::rename(fk_subjects_id = id)
    doc_sheet_list$Subjects = doc_sheet_list$Subjects %>%
      left_join(match_entry)
  } else {
    doc_sheet_list$Subjects = doc_sheet_list$Subjects %>%
      left_join(get_tbl_id("subjects", idFilter="") %>%
                  rename(fk_subjects_id = id))  
  }
  
##############################################################################
####Push Series Sheet to CvT (matching to fk_study_id and fk_subject_id)
##############################################################################
  #Filter to only series of interest from normalization QA
  load_series = normalization_log %>%
    filter(filename == gsub(".xlsx", "", basename(f)),
           load_series == 1) %>%
    select(series_id) 
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    filter(id %in% load_series$series_id)
  
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
  
  #MATCH TO CHEMICALS TABLE
  if(show_chemical_match){
    cvt_chemicals = readxl::read_xlsx("/home/jwall01/cvtdb/input/chemicals/PBPKShowaPharma_238mapped_07Dec2021.xlsx") %>%
      select(raw_name=`Query Name`, raw_cas = `Query Casrn`, dsstox_substance_id = `Top HIT DSSTox_Substance_Id`, Validated) %>%
      filter(!is.na(dsstox_substance_id)) %>%
      distinct()
    
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      select(-dsstox_substance_id, -dsstox_casrn, -preferred_name) %>%
      left_join(cvt_chemicals, by=c("analyte_name_original"="raw_name", 
                                    "analyte_casrn_original"="raw_cas")) %>%
      filter(Validated != FALSE) %>% #Filter out invalidated chemical series until they're validated
      select(-Validated)
    
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      left_join(query_cvt("SELECT id as fk_analyzed_chemical_id, dsstox_substance_id FROM cvt.chemicals"),
                by="dsstox_substance_id")
    
  } else {
    cvt_chemicals = query_cvt("SELECT id As fk_analyzed_chemical_id, dsstox_substance_id, dsstox_casrn, preferred_name FROM cvt.chemicals")
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      left_join(cvt_chemicals, by=c("dsstox_substance_id", "dsstox_casrn", "preferred_name"))
    #Filter to ones that didn't match, add new chemicals table entry
    chem_push = doc_sheet_list$Series %>%
      filter(is.na(fk_analyzed_chemical_id), !is.na(dsstox_substance_id)) %>%
      select(dsstox_substance_id, dsstox_casrn, preferred_name) %>%
      mutate(chemistry_team_mapping = 0) %>%
      distinct()
    if(nrow(chem_push)){
      message("INSERT LOGIC TO ADD NEW CHEMICAL RECORD AND REMATCH fk_dose_chemical_id")
    }
  }
  
  ###Conc_Medium Matching###
  match_check = nrow(doc_sheet_list$Series)
  
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    select(-conc_medium, -conc_medium_normalized, -conc_medium_id) %>% #remove if already present, matching here now
    left_join(query_cvt("SELECT * FROM cvt.conc_medium_dict") %>%
                select(fk_conc_medium_id = id, conc_medium_original),
              by=c("conc_medium_original"))
  
  if(match_check != nrow(doc_sheet_list$Series)){
    stop("...Concentration Medium matching produced duplicates...check")
  }
  
  message("...pushing to Series table")
  #xmin onward for int conversion
  push_to_CvT(df=doc_sheet_list$Series %>%
                select(-dsstox_substance_id, -dsstox_substance_id, -chemistry_team_mapping),
              tblName="series")
  #Get Series ID Values (Assumes the query returns the rows in the same order they were uploaded...)
  doc_sheet_list$Series = doc_sheet_list$Series %>%
    left_join(get_tbl_id("Series", idFilter="") %>% #Left join so it only joins what records match
                rename(new_fk_series_id = id) %>%
                mutate(across(c(loq, n_subjects_in_series), as.numeric)))

  # ####Push Conc_Time_Values to CvT (matching to fk_series_id)
  #Filter to only series of interest from normalization QA
  doc_sheet_list$Conc_Time_Values = doc_sheet_list$Conc_Time_Values %>%
    filter(fk_series_id %in% doc_sheet_list$Series$id)
  message("...pushing to Conc_Time_Values table")
  push_to_CvT(df=doc_sheet_list$Conc_Time_Values %>%
                left_join(doc_sheet_list$Series %>% select(fk_series_id=id, new_fk_series_id), by=c("fk_series_id")) %>%
                mutate(fk_series_id = new_fk_series_id) %>%
                select(-new_fk_series_id, -id, -species),
              tblName="conc_time_values")
  
    #NEED TO SET documents table "extracted" to 1
  writexl::write_xlsx(doc_sheet_list, path=paste0("output/", 
                                                  basename(f) %>% gsub(".xlsx", "", .), 
                                                  "_loaded_", format(Sys.time(), "%Y%m%d"), 
                                                  ".xlsx"))
  
  #insert logic to update the series load log for loaded series
}
message("Done...", Sys.time())

#con = connect_to_CvT()
#dbListTables(con)