#Find duplicates in documents table based on ID hierarchy
#Return associated CvTdb records to help compare between potential duplicates
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
invisible(sapply(file_source, source,.GlobalEnv))
#Input list of ID values to check in hierarchical order
check_list = c("pmid", "other_study_identifier", "doi", "url")
#Find potential duplicate values per ID level (irrespective of the hierarchy)
where_clause = lapply(check_list, function(x){
  tmp = query_cvt(paste0("SELECT ",x," from cvt.documents")) %>%
    filter(!is.na(!!sym(x))) %>% unlist() %>% unname()
  tmp = tmp[duplicated(tmp)] %>% unique() %>% paste0(collapse="', '")
}) %T>% { names(.) <- check_list }
#Pull all document data
input = query_cvt("SELECT * FROM cvt.documents")
#Loop through the hierarchy to verify potenital duplicates
doc_list = list()
for(level in check_list){#Check each level, then filter out matched and to those missing a level entry
  if(!stringr::str_length(where_clause[[level]])){
    #No duplicates identified in search level, so filter out those with such values
    input = input %>% 
      filter(!id %in% doc_list[[level]]$id, #Filter out identified duplicates
             is.na(!!sym(level))) #Filter to missing from this level
    next #Skip empty search
  }
  doc_list[[level]] = query_cvt(paste0("SELECT * FROM cvt.documents where ", 
                                            level, " in ('", where_clause[[level]],"')"))
  doc_list[[level]] = input %>%
    filter(!!sym(level) %in% where_clause[[level]]) %>%
    mutate(doc_name = paste0(!!sym(level), "_", level,"_doc_id_", id)) %>%
    select(id, !!sym(level), doc_name)
  input = input %>% 
    filter(!id %in% doc_list[[level]]$id, #Filter out identified duplicates
           is.na(!!sym(level))) #Filter to missing from this level
}

doc_list = bind_rows(doc_list) %>% arrange(doc_name)
out = lapply(seq_len(nrow(doc_list)), function(r){
  message("Pulling data for row: ", r, " ID: ", doc_list$id[r])
  #Get document sheet
  document = query_cvt(paste0("SELECT * FROM cvt.documents WHERE id = ", doc_list$id[r])) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  #Get associated studies
  study = query_cvt(paste0("SELECT * from cvt.studies where fk_extraction_document_id = ", doc_list$id[r],
                                " OR fk_reference_document_id = ", doc_list$id[r])) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  #Return empty if no studies associated wtih document
  if(!nrow(study)) { message("...no studies associated with document..."); return("...no studies associated with document...") }
  #Get associated series
  series = query_cvt(paste0("SELECT * from cvt.series where fk_study_id in (", 
                                 toString(study$id),")")) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  
  if(!nrow(series)) { message("...no series associated with document..."); return("...no series associated with document...") }
  #Get subject data
  if(!length(series$fk_subject_id[!is.na(series$fk_subject_id)])) { message("...CURATION ERROR: no subjects associated with series..."); return("...CURATION ERROR: no subjects associated with series...") }
  subjects = query_cvt(paste0("SELECT * FROM cvt.subjects where id in (",
                              toString(series$fk_subject_id), ")")) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  #Get conc data
  conc = query_cvt(paste0("SELECT * from cvt.conc_time_values where fk_series_id in (", 
                          toString(series$id),")")) %>%
    select(-ends_with("_by"), -ends_with("_dt"))
  
  #Return data
  tmp = series %>%
    left_join(study, by=c("fk_study_id"="id")) %>%
    left_join(subjects, by=c("fk_subject_id"="id")) %>%
    left_join(conc, by=c("id"="fk_series_id"))
    
}) %T>% { names(.) <- doc_list$doc_name } 

message("Number of documents without check_list identifiers: ", nrow(input))