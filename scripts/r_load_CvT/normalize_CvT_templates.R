# Script to push CvT extracted docs to CvT database
# Created by: Jonathan Taylor Wall
# Created Date: 2021-02-25
# R version 4.1.0 (2021-05-18)
# httk_2.1.0; readxl_1.3.1; tidyr_1.2.0; magrittr_2.0.2; dplyr_1.0.8; DBI_1.1.2
# purrr_0.3.4; assertthat_0.2.1

# # Load packages
# require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl); require(validate); library(httk)
# # Load R Scripts
# devtools::load_all()

################################################################################
###Main Script Section
################################################################################

# inputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/Format QA/0_to_qa_format/Needs Admin Check"#"QA Complete/"#"../QA CvT/QA Complete/"
# inputDir = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2022\\PKWG-CompletedTemplates\\CvT_completed_templates\\PFAS_PIP"
normalize_CvT_templates <- function(file_location = "clowder",
                                    inputDir,
                                    template_path = "input/CvT_data_template_articles.xlsx",
                                    sheetList = c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values"),
                                    curated_chemicals = "input/chemicals/curated_chemicals_comparison_2021-11-23.xlsx",
                                    apiKey = Sys.getenv("apiKey"),
                                    baseurl = Sys.getenv("baseurl"),
                                    file_dsID = Sys.getenv("file_dsID"),
                                    doc_dsID = Sys.getenv("doc_dsID")
                                    ){
  # Check for Clowder ID values
  if(is.null(apiKey) | is.null(doc_dsID) | is.null(baseurl)) stop("Must provide apiKey, baseurl, and doc_dsID (document dataset ID) to match Clowder Documents.")
  if(apiKey == "" | doc_dsID == "" | baseurl == "") stop("Must provide apiKey, baseurl, and doc_dsID (document dataset ID) to match Clowder Documents.")
  if(is.null(file_location) || !file_location %in% c("clowder", "local")) stop("Must provide file_location of either 'clowder' or 'local'.")
  ###########################
  #Push to CvT
  ###########################
  # Push 1 document at a time because of the need to pull unique ID values auto-generated
  # when pushed to a database table
  
  if(file_location == "local"){
    if(is.null(inputDir) || is.na(inputDir)) stop("If file_location is 'local', must provide an 'inputDir' to pull files from.")
    fileList = list.files(inputDir, full.names = TRUE, pattern=".xlsx", recursive=TRUE)
    fileList = fileList[!grepl("~|_normalize|Needs Admin|Needs Further|Reviewer Dis|needs_edits|Copy of|_log|template_metadata|Rejected|pdfs", fileList)] #Remove tmp files
    if(!length(fileList)){
      return("No local files found in inputDir to normalize...")
    }
    fileList = data.frame(filename = fileList)
  } else {
    # Pull Clowder information
    if(is.null(file_dsID) || is.na(file_dsID)) stop("If file_location is 'clowder', must provide a 'file_dsID' to pull files from.")
    fileList = pull_clowder_files_to_load(dsID=file_dsID, baseurl=baseurl, apiKey=apiKey)
    if(!nrow(fileList)){
      return("No clowder files found in file_dsID to normalize...")
    }
  }
  
  # Set up output folder structure
  if(!dir.exists("output/normalized_templates")){
    if(!dir.exists("output")) dir.create("output")
    dir.create("output/normalized_templates")
    dir.create("output/normalized_templates/flagged")
    dir.create("output/normalized_templates/flagged/warning")
    dir.create("output/normalized_templates/flagged/hard_stop")
    dir.create("output/normalized_templates/flagged/hard_stop/missing_required")
    dir.create("output/normalized_templates/flagged/hard_stop/need_split")
    dir.create("output/normalized_templates/flagged/hard_stop/impossible_value")
    dir.create("output/normalized_templates/flagged/hard_stop/conversion_failed")
    dir.create("output/normalized_templates/flagged/hard_stop/empty_sheet")
    dir.create("output/normalized_templates/flagged/soft_stop")
    dir.create("output/normalized_templates/flagged/soft_stop/conversion_needed")
    dir.create("output/normalized_templates/flagged/soft_stop/dictionary_update")
    dir.create("output/normalized_templates/flagged/soft_stop/clowder_missing")
  }
  
  for(i in seq_len(nrow(fileList))){
    # if(i <= 85){#Quick skip/restart logic
    #   next
    # }
    f = fileList$filename[i]
    #Skip already normalized template
    if(file.exists(paste0("output/normalized_templates/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/warning/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/hard_stop/missing_required/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/hard_stop/need_split/", gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/hard_stop/impossible_value/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/hard_stop/conversion_failed/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/hard_stop/empty_sheet/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/soft_stop/conversion_needed/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/soft_stop/dictionay_update/",gsub(".xlsx", "_normalized.xlsx", basename(f)))) |
       file.exists(paste0("output/normalized_templates/flagged/soft_stop/clowder_missing/",gsub(".xlsx", "_normalized.xlsx", basename(f))))
    ){
      next
    }
    #Skip problem files (for now)
    # if(f %in% c("L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/Format QA/1_qa_format_complete/HERO7578028_CvT_data_template_articles_RRS.xlsx")){
    #   next
    # }
    ######insert loop over fileList logic########
    message("Normalizing file (", i, "/", nrow(fileList),"): ", f, "...", Sys.time())
    
    #Load Documents Sheet
    if(file_location == "local"){
      doc_sheet_list = load_sheet_group(fileName = f, template_path = template_path)
    } else {
      # # Pull temp file to process
      doc_sheet_list = load_file_from_api(url = paste0(baseurl,"/api/files/",fileList$clowder_id[i],"/blob"),
                               headers = c(`X-API-Key` = apiKey),
                               mode = "wb",
                               file_type = "xlsx")
    }
    
    #Create/clear log entry for filename
    log_CvT_doc_load(f, m=NULL, reset=TRUE)
    
    # Check for empty template sheets
    if(check_empty_sheet(doc_sheet_list)){
      log_CvT_doc_load(f, m="empty_sheet")  
      next
    }
    
    # Log file_location
    log_CvT_doc_load(f, m="file_location", 
                     val=ifelse(file_location=="clowder", 
                                paste0(file_location, ": ", fileList$clowder_id[i]), 
                                file_location))
    #Check if file already loaded
    #
    #
    #
    #NEED TO UPDATE LOGIC TO HANDLE MULTIPLE DOCUMENTS (extraction and reference) uploads for a template
    #
    #
    #
    # doc_check = db_query_cvt(paste0("SELECT pmid FROM documents where pmid in ('", 
    #                              doc_sheet_list$Documents$pmid, " AND extracted != 0')"))
    # 
    # if(nrow(doc_check)){
    #   message("...File already pushed...skipping")
    #   log_CvT_doc_load(f, m="already_loaded")
    #   next
    # }
    
    # TODO - Improve Clowder ID mapping logic (case where template has clowder_id field)
    # Match to Clowder documents
    doc_sheet_list$Documents=clowder_match_docs(df=doc_sheet_list$Documents,
                                                dsID=doc_dsID,
                                                baseurl=baseurl,
                                                apiKey=apiKey)
    
    if(any(is.na(doc_sheet_list$Documents$clowder_file_id))){
      log_CvT_doc_load(f, m="missing_clowder_file_ids")
    }
    
    #Normalize species
    doc_sheet_list$Subjects = normalize_species(x=doc_sheet_list$Subjects)
    
    #Normalize administration route (use dictionary to map)
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::rename(administration_route_original = administration_route) %>%
      mutate(administration_route_original = tolower(administration_route_original)) %>%
      left_join(readxl::read_xlsx("input\\dictionaries\\administration_route_dict.xlsx") %>%
                  dplyr::rename(fk_administration_route = id),
                by="administration_route_original")
    
    #Check Species
    species_check = db_query_cvt(paste0("SELECT DISTINCT species FROM cvt.subjects"))
    if(any(!doc_sheet_list$Subjects$species %in% species_check$species)){
      message("...File contains species not already in database: ", doc_sheet_list$Subjects$species[!doc_sheet_list$Subjects$species %in% species_check$species])
      log_CvT_doc_load(f, m="species_not_found")
      #next
    }
    
    #Check if file contains all expected sheets
    if(any(!sheetList %in% names(doc_sheet_list))){
      message("...File missing sheet: ", paste0(sheetList[!sheetList %in% names(doc_sheet_list)], collapse = ", "), "...skipping...")
      log_CvT_doc_load(f, m="missing_sheets")
      next
    }
    
    #Check if more than 1 document --> deprecated since we allow reference_documents now
    # if(nrow(doc_sheet_list$Documents) > 1){
    #   message("...File contains more than 1 document...skipping...")
    #   log_CvT_doc_load(f, m="multiple_docs")
    #   next
    # }
    
    #Match curated chemicals - rename columns to generic names 
    tmp = chemical_curation_match_curated_chemicals(df=doc_sheet_list$Studies %>%
                                    select(name=test_substance_name, 
                                           name_secondary=test_substance_name_secondary, 
                                           casrn=test_substance_casrn), 
                                  f_name=curated_chemicals)
    doc_sheet_list$Studies = cbind(doc_sheet_list$Studies, tmp)
    
    #Match curated chemicals - rename columns to generic names 
    tmp = chemical_curation_match_curated_chemicals(df=doc_sheet_list$Series %>%
                                    select(name=analyte_name, 
                                           name_secondary=analyte_name_secondary, 
                                           casrn=analyte_casrn), 
                                  f_name=curated_chemicals)
    doc_sheet_list$Series = cbind(doc_sheet_list$Series, tmp)
    
    #Call to the orchestration function for data normalization (with error logging)
    doc_sheet_list = normalize_CvT_data(df=doc_sheet_list, f=f)
    
    #Check if normalized data has all required fields (and no NA missing values in required fields)
    #check_required_fields(df=doc_sheet_list, f=f)
    check_required_fields_validator(df=doc_sheet_list, f=f)
    
    #Rename columns
    doc_sheet_list$Studies = doc_sheet_list$Studies %>%
      dplyr::rename(test_substance_name_original = test_substance_name,
                    test_substance_name_secondary_original = test_substance_name_secondary,
                    test_substance_casrn_original = test_substance_casrn,
                    dose_level_original = dose_level,
                    dose_level_original_units = dose_level_units)
    doc_sheet_list$Series = doc_sheet_list$Series %>%
      dplyr::rename(analyte_name_original = analyte_name,
                    analyte_name_secondary_original = analyte_name_secondary,
                    analyte_casrn_original = analyte_casrn,
                    time_units_original = time_units,
                    conc_units_original=conc_units)
    
    #Cache normalized template
    save_normalized_template(df=doc_sheet_list, f=f)
    
    # #If any issues were logged during normalization, don't push the doc
    # if(log_check(basename(f))){
    #   message("...file has logged issues...skipping doc")
    #   next
    # }
  }
  
  message("Sorting files")
  reorganize_file_flags()
  
  message("Done...", Sys.time())
}
