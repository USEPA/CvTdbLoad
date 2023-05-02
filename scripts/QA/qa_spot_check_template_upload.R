#QA Script to spot check 20% of documents pushed with a template
#Created By: Jonathan Taylor Wall
#Created Date: 2022-02-22
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
invisible(sapply(file_source, source,.GlobalEnv))
#Load input template (filter/transform)

#Load cached output from template load

#Sample documents (use all if under 5 documents)

#Compare input vs. output (ignore foreign keys) for generic "do record counts match" check

#Manual comparison for foreign key matches

stop("Adapt the below code for the above use cases")

out = lapply(seq_len(nrow(doc_list)), function(r){
  message("Pulling data for row: ", r, " ID: ", doc_list$id[r])
  #Get document sheet
  document = db_query_cvt(paste0("SELECT * FROM cvt.documents WHERE id = ", doc_list$id[r])) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  #Get associated studies
  study = db_query_cvt(paste0("SELECT * from cvt.studies where fk_extraction_document_id = ", doc_list$id[r],
                           " OR fk_reference_document_id = ", doc_list$id[r])) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  #Return empty if no studies associated wtih document
  if(!nrow(study)) { message("...no studies associated with document..."); return("...no studies associated with document...") }
  #Get associated series
  series = db_query_cvt(paste0("SELECT * from cvt.series where fk_study_id in (", 
                            toString(study$id),")")) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  
  if(!nrow(series)) { message("...no series associated with document..."); return("...no series associated with document...") }
  #Get subject data
  if(!length(series$fk_subject_id[!is.na(series$fk_subject_id)])) { message("...CURATION ERROR: no subjects associated with series..."); return("...CURATION ERROR: no subjects associated with series...") }
  subjects = db_query_cvt(paste0("SELECT * FROM cvt.subjects where id in (",
                              toString(series$fk_subject_id), ")")) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  #Get conc data
  conc = db_query_cvt(paste0("SELECT * from cvt.conc_time_values where fk_series_id in (", 
                          toString(series$id),")")) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  
  #Return data
  tmp = series %>%
    left_join(study, by=c("fk_study_id"="id")) %>%
    left_join(subjects, by=c("fk_subject_id"="id")) %>%
    left_join(conc, by=c("id"="fk_series_id"))
  
}) %T>% { names(.) <- doc_list$doc_name } 