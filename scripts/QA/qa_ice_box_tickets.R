#Script to check cvtdb vs. new templates for duplicate entries before curation (Ice Box tickets)
#Created by: Jonathan Taylor Wall
#Created Date: 2022-12-2
#Load packages
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
invisible(sapply(file_source, source,.GlobalEnv))

# Query to see if any CvT documents are a potential match
query_potential_dup <- function(dup_dat = NULL){
  
  # Get potential series
  series_id = query_cvt(paste0("SELECT * FROM cvt.conc_time_values where concat(time_original, '_', conc_original) in ('", 
                               paste0(dup_dat, collapse="', '"), 
                               "') or concat(time_hr, '_', conc_original) in ('", 
                               paste0(dup_dat, collapse="', '"), 
                               "')")) %>%
    unique()
  if(!nrow(series_id)){
    return(NA)
  }
  # Get potential studies
  study_id = query_cvt(paste0("SELECT DISTINCT fk_study_id FROM cvt.series WHERE id in ('", 
                                 paste0(series_id$fk_series_id, collapse = "', '"), "')"))
  # Get potential documents
  doc_id = query_cvt(paste0("SELECT DISTINCT fk_extraction_document_id FROM cvt.studies WHERE id in ('", 
                              paste0(study_id$fk_study_id, collapse = "', '"), "')"))
  
  return(doc_id$fk_extraction_document_id)
}

# Pull potential matching documents in a nested template list
get_potential_dup <- function(doc_list){
  lapply(seq_len(length(doc_list)), function(doc){
    message("Pulling document: ", doc, " of ", length(doc_list))
    doc = doc_list[doc]
    tmp = list()
    tmp$documents = query_cvt(paste0("SELECT * FROM cvt.documents where id = ", doc))
    tmp$studies = query_cvt(paste0("SELECT * FROM cvt.studies where fk_extraction_document_id = ", doc))
    tmp$series = query_cvt(paste0("SELECT * FROM cvt.series where fk_study_id in ('", 
                                  paste0(tmp$studies$id, collapse = "', '"), "')"))
    tmp$subjects = query_cvt(paste0("SELECT * FROM cvt.subjects where id in ('", 
                                    paste0(unique(tmp$series$fk_subject_id), collapse="', '"),"')"))
    tmp$conc_time_values = query_cvt(paste0("SELECT * FROM cvt.conc_time_values where fk_series_id in ('", 
                                            paste0(tmp$series$id, collapse="', '"),"')"))
    return(tmp)
  }) %T>% {
    names(.) <- doc_list
  } %>% return()
}

run_dup_comparison <- function(f = NULL, limit = 50, template_path = NULL){
  # CvT Template path
  # template_path = "L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/CvT_data_template_articles.xlsx"
  
  # Load template of interest
  # f = "L:\\Lab\\HEM\\T_Wall_Projects_FY20\\cvtdb\\CvT Curation\\Ice Box\\CVTDB-15\\NTP-DATA-002-00037-0002-000-4_HERO3861408_CvT_data_template_articles_jwall01_1.xlsx"
  tmp_data = load_sheet_group(fileName = f, template_path = template_path) 
  dup_dat = tmp_data$Conc_Time_Values %>%
    tidyr::unite("comp", time, conc) %>%
    select(comp) %>%
    unique() %>% unlist() %>% unname()
  # Chunk query to get candidate documents
  groups = split(dup_dat, ceiling(seq_along(dup_dat)/limit))
  # Run query to pull candidate list
  doc_list = lapply(seq_len(length(groups)), function(group){
    message("Querying group: ", group, " of ", length(groups))
    query_potential_dup(groups[[group]]) %>%
      return()
  }) %>% unlist() %>% unique() %>%
    .[!is.na(.)] %>%
    as.numeric() %>%
    sort()
  # Run query to pull list of template documents
  return(get_potential_dup(doc_list))
}

#####################################################################################
out = run_dup_comparison(f = "L:\\Lab\\HEM\\T_Wall_Projects_FY20\\cvtdb\\CvT Curation\\Ice Box\\CVTDB-59\\CVTDB59_raw_dup_test_20221215.xlsx",
                         template_path = "L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/CvT_data_template_articles.xlsx")
save(out, file="output/CVTDB##_candidate_dups_20221215.RData")

docs = lapply(out, function(doc){
  doc$documents
}) %>% dplyr::bind_rows()

View(docs)
