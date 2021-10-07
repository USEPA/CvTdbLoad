#Script to push CvT extracted docs to CvT database
#Created by: Jonathan Taylor Wall
#Created Date: 2021-02-25
#Load packages
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="*.R", full.names = TRUE)
invisible(sapply(file_source[!grepl("get_unique_chemicals|_dict|log_", file_source)],source,.GlobalEnv))
#load("cached_cvt_screening_20210922.RData")
################################################################################
###Main Script Section
################################################################################
#apiKey = Sys.getenv("apiKey")
#clowder_dataset = "CvT Raw Extracted"
#setwd("C:/Users/JWALL01/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/CvT PDI/QA CvT")
#outputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA/0_to_qa_format/Needs Admin Check"#"QA Complete/"#"../QA CvT/QA Complete/"
outputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA/1_qa_format_complete"
template_path = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/CvT_data_template_articles.xlsx"
sheetList = c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values")
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

#initialize_CvTdb()
#initialize_CvTdb_from_RDat("L:/Lab/HEM/T_Wall_Projects_FY20/CvT Database/CvT_dump_20210825.RData")
fileList = list.files(outputDir, full.names = TRUE, pattern=".xlsx")
fileList = fileList[!grepl("~", fileList)] #Remove tmp files

for(i in seq_len(length(fileList))){
  f = fileList[i]
  ######insert loop over fileList logic########
  message("Pushing file (", i, "/", length(fileList),"): ", f, "...", Sys.time())
  #Create/clear log entry for filename
  log_CvT_doc_load(f, m=NULL, reset=TRUE)
  #Load Documents Sheet
  doc_sheet_list = load_sheet_group(fileName = f, template_path = template_path) 
  #Check if file already loaded
  doc_check = query_cvt(paste0("SELECT pmid FROM documents where pmid in ('", 
                               doc_sheet_list$Documents$pmid, " AND extracted != 0')"))
  
  if(nrow(doc_check)){
    message("...File already pushed...skipping")
    log_CvT_doc_load(f, m="already_loaded")
    next
  }
  
  doc_sheet_list$Subjects$species = normalize_species(x=doc_sheet_list$Subjects$species)
  
  species_check = query_cvt(paste0("SELECT DISTINCT species FROM subjects"))
  if(any(!doc_sheet_list$Subjects$species %in% species_check$species)){
    message("...File contains species not already in database: ", doc_sheet_list$Subjects$species[!doc_sheet_list$Subjects$species %in% species_check$species])
    log_CvT_doc_load(f, m="species_not_found")
    next
  }
  
  #Check if file contains all expected sheets
  if(any(!sheetList %in% names(doc_sheet_list))){
    message("...File missing sheet: ", paste0(sheetList[!sheetList %in% names(doc_sheet_list)], collapse = ", "), "...skipping...")
    log_CvT_doc_load(f, m="missing_sheets")
    next
  }
  #Check if more than 1 document
  if(nrow(doc_sheet_list$Documents) > 1){
    message("...File contains more than 1 document...skipping...")
    log_CvT_doc_load(f, m="multiple_docs")
    next
  }
  
  #Call to the orchestration function for data normalization (with error logging)
  doc_sheet_list = normalize_CvT_data(df=doc_sheet_list, f=f)
  
  #If any issues were logged during normalization, don't push the doc
  if(log_check(basename(f))){
    message("...file has logged issues...skipping doc")
    next
  }
}{
# #^^^Remove these open/close brackets when ready to run the full load workflow...
# ####Push Documents Sheet to CvT
#   message("...pushing to Documents table")
# #^^^Remove this close bracket when ready to run the full load workflow...
#   push_to_CvT(df=doc_sheet_list$Documents %>%
#                 select(pmid, doi, first_author, year, title),
#               tblName="Documents")
#   #Get Documents ID values - JOin back to original data for matching
#   doc_sheet_list$Documents = doc_sheet_list$Documents %>%
#     left_join(get_tbl_id("Documents", 
#                          idFilter=paste0("WHERE pmid IN (", 
#                                          toString(ifelse(!is.na(doc_sheet_list$Documents$pmid), doc_sheet_list$Documents$pmid, "")), 
#                                          ") OR other_study_identifier IN (",
#                                          toString(ifelse(!is.na(doc_sheet_list$Documents$other_study_identifier),
#                                                          doc_sheet_list$Documents$other_study_identifier, "")), ")")) %>%
#                 rename(fk_document_id = id))
# ####Push Studies Sheet to CvT (after adding fk_extraction_document_id from idList)
#   doc_sheet_list$Studies = doc_sheet_list$Studies %>% #Directly assigning
#     mutate(fk_extraction_document_id = doc_sheet_list$Documents$fk_document_id)
#   message("...pushing to Studies table")
#   push_to_CvT(df=doc_sheet_list$Studies, tblName="Studies")
#   
# ####Push Subjects Sheet to CvT (no fk to add)
#   message("...pushing to Subjects table")
#   push_to_CvT(df=doc_sheet_list$Subjects, tblName="Subjects")
#   #Get Studies ID values (Assumes the query returns the rows in the same order they were uploaded...)
#   doc_sheet_list$Studies = doc_sheet_list$Studies %>% 
#     mutate(fk_extraction_document_id = as.character(fk_extraction_document_id)) %>%
#     left_join(get_tbl_id("Studies", #Left join so it only joins what records match
#                          idFilter=paste0("WHERE fk_extraction_document_id IN (", 
#                                          toString(doc_sheet_list$Documents$fk_document_id), ")")) %>%
#                 rename(fk_studies_id = id))
#   #Get Subjects ID Values (Assumes the query returns the rows in the same order they were uploaded...)
#   doc_sheet_list$Subjects = doc_sheet_list$Subjects %>%
#     left_join(get_tbl_id("Subjects", idFilter="") %>%
#                 rename(fk_subjects_id = id))
# 
# ####Push Series Sheet to CvT (matching to fk_study_id and fk_subject_id)
#   doc_sheet_list$Series = doc_sheet_list$Series %>%
#     left_join(doc_sheet_list$Studies %>% #Left join so it only joins what records match
#                 select(fk_study_id=id, new_fk_study_id=fk_studies_id), 
#               by="fk_study_id") %>%
#     left_join(doc_sheet_list$Subjects %>% #Left join so it only joins what records match
#                 select(fk_subject_id=id, new_fk_subject_id=fk_subjects_id), 
#               by="fk_subject_id") %>%
#     mutate(fk_study_id = new_fk_study_id,
#            fk_subject_id = new_fk_subject_id) %>%
#     select(-new_fk_subject_id, -new_fk_study_id)
#   message("...pushing to Series table")
#   push_to_CvT(df=doc_sheet_list$Series, 
#               tblName="Series")
#   #Get Series ID Values (Assumes the query returns the rows in the same order they were uploaded...)
#   doc_sheet_list$Series = doc_sheet_list$Series %>%
#     left_join(get_tbl_id("Series", idFilter="") %>% #Left join so it only joins what records match
#                 rename(new_fk_series_id = id))
# 
# ####Push Conc_Time_Values to CvT (matching to fk_series_id)
#   message("...pushing to Conc_Time_Values table")
#   push_to_CvT(df=doc_sheet_list$Conc_Time_Values %>%
#                 left_join(doc_sheet_list$Series %>% select(fk_series_id=id, new_fk_series_id), by=c("fk_series_id")) %>%
#                 mutate(fk_series_id = new_fk_series_id) %>%
#                 select(-new_fk_series_id), 
#               tblName="Conc_Time_Values")
}
message("Done...", Sys.time())

#con = connect_to_CvT()
#dbListTables(con)