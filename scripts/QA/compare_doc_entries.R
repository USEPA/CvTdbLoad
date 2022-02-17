library(DBI); library(dbplyr)
#Script to compare duplicate CvT document entry Conc_Time_Values
check_list = query_cvt("SELECT pmid from cvt.documents") %>%
  group_by(pmid) %>%
  summarise(n=n()) %>%
  filter(n > 1, !is.na(pmid)) %>%
  select(pmid) %>% unlist() %>% unname()

doc_list = query_cvt(paste0("SELECT id, pmid FROM cvt.documents where pmid in (", toString(check_list),")")) %>%
  mutate(doc_name = paste(pmid, id, sep="_")) %>%
  arrange(doc_name)

out = lapply(seq_len(nrow(doc_list)), function(r){
  #Get associated studies
  study_list = query_cvt(paste0("SELECT distinct id from cvt.studies where fk_extraction_document_id = ", doc_list$id[r],
                                " OR fk_reference_document_id = ", doc_list$id[r])) %>%
    unlist() %>% unname()
  if(!length(study_list)) return(data.frame())
  #Get associated series
  series_list = query_cvt(paste0("SELECT distinct id from cvt.series where fk_study_id in (", 
                                 toString(study_list),")")) %>%
    unlist() %>% unname()
  if(!length(series_list)) return(data.frame())
  #Get conc data
  dat = query_cvt(paste0("SELECT * from cvt.conc_time_values where fk_series_id in (", toString(series_list),")"))
}) %T>% {names(.) <- doc_list$doc_name } #%>%
  #purrr::compact()
