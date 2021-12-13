#Load original chemical list and CompTox Batch Search lists
date = "2021-11-23"
out = readxl::read_xlsx(paste0("input/chemicals/cvt_to_curate_unique_chemicals_",date,".xlsx"))
batch_casrn = tryCatch(readxl::read_xls(paste0("input/chemicals/CompToxChemicalsDashboard-Batch-Search_",date,"_casrn.xls")), error=function(cond){ return(data.frame(INPUT=as.character(),
                                                                                                                                                                      FOUND_BY=as.character(),
                                                                                                                                                                      DTXSID=as.character())) })
batch_full = tryCatch(readxl::read_xls(paste0("input/chemicals/CompToxChemicalsDashboard-Batch-Search_",date,"_full_name.xls")), error=function(cond){ return(data.frame(INPUT=as.character(),
                                                                                                                                                                         FOUND_BY=as.character(),
                                                                                                                                                                         DTXSID=as.character())) })
batch_no_par = tryCatch(readxl::read_xls(paste0("input/chemicals/CompToxChemicalsDashboard-Batch-Search_",date,"_no_parentheses.xls")), error=function(cond){ return(data.frame(INPUT=as.character(),
                                                                                                                                                                                FOUND_BY=as.character(),
                                                                                                                                                                                DTXSID=as.character())) })

##################
####Check and combine chemical name batch matches
##################
#Match 1 - Full name
out_matched_full = out %>%
  left_join(batch_full %>% select(INPUT, FOUND_BY, DTXSID),
            by=c("name"="INPUT")) %>%
  distinct()
#Match 2 (unmatched from 1) - No parentheses name
out_matched_no_par = out_matched_full %>% 
  filter(FOUND_BY == "NO_MATCH") %>%
  select(-FOUND_BY, -DTXSID) %>%
  left_join(batch_no_par %>% select(INPUT, FOUND_BY, DTXSID),
            by=c("name_no_parentheses"="INPUT")) %>%
  distinct()
#Filter to only matched records
out_matched_full = out_matched_full %>%
  filter(FOUND_BY != "NO_MATCH")

#Check match count
# rbind(out_matched_full, out_matched_no_par) %>%
#   group_by(FOUND_BY) %>% 
#   summarise(n=n()) %>% 
#   View()

#UUID check - more rows due to multiple matches found on CCD
# cbind(out %>% filter(uuid %in% out_matched_full$uuid) %>% dplyr::rename(uuid_before=uuid) %>% group_by(uuid_before) %>% summarise(n_before = n()),
#       out_matched_full %>% dplyr::rename(uuid_after=uuid) %>% group_by(uuid_after) %>% summarise(n_after=n())) %>%
#   filter(n_before != n_after) %>%
#   View()

#UUID check - more rows due to multiple matches found on CCD
# cbind(out %>% filter(uuid %in% out_matched_no_par$uuid) %>% dplyr::rename(uuid_before=uuid) %>% group_by(uuid_before) %>% summarise(n_before = n()),
#       out_matched_no_par %>% dplyr::rename(uuid_after=uuid) %>% group_by(uuid_after) %>% summarise(n_after=n())) %>%
#   filter(n_before != n_after) %>%
#   View()

#Get final result - combined name batch search
batch_matched_name_combined = rbind(out_matched_full, out_matched_no_par) %>%
  select(uuid, chem_type, DTXSID) %>%
  distinct()

##################
####Check and combine CASRN batch matches
##################
#Match 1 - CASRN
out_matched_casrn = out %>%
  left_join(batch_casrn %>% select(INPUT, FOUND_BY, DTXSID),
            by=c("casrn"="INPUT")) %>%
  select(uuid, chem_type, DTXSID) %>%
  distinct()

#Combine name and casrn batch matches - only accept when they match
compare_batch = out_matched_casrn %>%
  left_join(batch_matched_name_combined, 
            by=c("uuid", "chem_type"),
            suffix=c("_casrn", "_name")) %>%
  mutate(accept_batch = DTXSID_casrn==DTXSID_name)

writexl::write_xlsx(list(orig_chem = out,
                         batch_casrn=batch_casrn,
                         batch_full=batch_full,
                         batch_no_par=batch_no_par,
                         compare_batch=compare_batch),
                    paste0("input/chemicals/curated_chemicals_comparison_", date, ".xlsx"))
