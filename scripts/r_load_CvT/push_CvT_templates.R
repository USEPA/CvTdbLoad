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
outputDir = "L:/Lab/HEM/T_Wall_Projects_FY20/CvT Database/output/normalized_templates"
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
  # ####Push Documents Sheet to CvT
    message("...pushing to Documents table")
    push_to_CvT(df=doc_sheet_list$Documents %>%
                  select(document_type, pmid, other_study_identifier, doi, 
                         first_author, year, title, url, curator_comment),
                tblName="Documents")
    #Get Documents ID values - JOin back to original data for matching
    doc_sheet_list$Documents = doc_sheet_list$Documents %>%
      left_join(get_tbl_id("Documents",
                           idFilter=paste0("WHERE pmid IN (",
                                           toString(ifelse(!is.na(doc_sheet_list$Documents$pmid), doc_sheet_list$Documents$pmid, "")),
                                           ") OR other_study_identifier IN (",
                                           toString(ifelse(!is.na(doc_sheet_list$Documents$other_study_identifier),
                                                           doc_sheet_list$Documents$other_study_identifier, "")), ")")) %>%
                  rename(fk_document_id = id))
  # ####Push Studies Sheet to CvT (after adding fk_extraction_document_id from idList)
    doc_sheet_list$Studies = doc_sheet_list$Studies %>% #Directly assigning
      mutate(fk_extraction_document_id = doc_sheet_list$Documents$fk_document_id)
    message("...pushing to Studies table")
    push_to_CvT(df=doc_sheet_list$Studies, tblName="Studies")
     
  # ####Push Subjects Sheet to CvT (no fk to add)
    message("...pushing to Subjects table")
    push_to_CvT(df=doc_sheet_list$Subjects, tblName="Subjects")
    #Get Studies ID values (Assumes the query returns the rows in the same order they were uploaded...)
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      mutate(fk_extraction_document_id = as.character(fk_extraction_document_id)) %>%
      left_join(get_tbl_id("Studies", #Left join so it only joins what records match
                           idFilter=paste0("WHERE fk_extraction_document_id IN (",
                                           toString(doc_sheet_list$Documents$fk_document_id), ")")) %>%
                  rename(fk_studies_id = id))
    #Get Subjects ID Values (Assumes the query returns the rows in the same order they were uploaded...)
    doc_sheet_list$Subjects = doc_sheet_list$Subjects %>%
      left_join(get_tbl_id("Subjects", idFilter="") %>%
                  rename(fk_subjects_id = id))

  # ####Push Series Sheet to CvT (matching to fk_study_id and fk_subject_id)
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      left_join(doc_sheet_list$Studies %>% #Left join so it only joins what records match
                  select(fk_study_id=id, new_fk_study_id=fk_studies_id),
                by="fk_study_id") %>%
      left_join(doc_sheet_list$Subjects %>% #Left join so it only joins what records match
                  select(fk_subject_id=id, new_fk_subject_id=fk_subjects_id),
                by="fk_subject_id") %>%
      mutate(fk_study_id = new_fk_study_id,
             fk_subject_id = new_fk_subject_id) %>%
      select(-new_fk_subject_id, -new_fk_study_id)
    message("...pushing to Series table")
    push_to_CvT(df=doc_sheet_list$Series,
                tblName="Series")
    #Get Series ID Values (Assumes the query returns the rows in the same order they were uploaded...)
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      left_join(get_tbl_id("Series", idFilter="") %>% #Left join so it only joins what records match
                  rename(new_fk_series_id = id))

  # ####Push Conc_Time_Values to CvT (matching to fk_series_id)
    message("...pushing to Conc_Time_Values table")
    push_to_CvT(df=doc_sheet_list$Conc_Time_Values %>%
                  left_join(doc_sheet_list$Series %>% select(fk_series_id=id, new_fk_series_id), by=c("fk_series_id")) %>%
                  mutate(fk_series_id = new_fk_series_id) %>%
                  select(-new_fk_series_id),
                tblName="Conc_Time_Values")
}
message("Done...", Sys.time())

#con = connect_to_CvT()
#dbListTables(con)