# # Script to explore output from qa_ice_box_tickets.R
# # Explore to determine if queued ticket is for a document already in CvTDB
# require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
# #Load R Scripts
# file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
# invisible(sapply(file_source, source,.GlobalEnv))
# # Load template of interest
# f = "L:\\Lab\\HEM\\T_Wall_Projects_FY20\\cvtdb\\CvT Curation\\Ice Box\\CVTDB-33\\Huang_5387170_PFHxS.xlsx"
# template_path = "L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/CvT_data_template_articles.xlsx"
# 
# tmp_data = load_sheet_group(fileName = f, 
#                             template_path = template_path)
# 
# load("L:\\Lab\\HEM\\T_Wall_Projects_FY20\\cvtdb\\CvT Curation\\Ice Box\\CVTDB-33\\CVTDB33_candidate_dups_20221202.RData")
# 
# # Compare test chemical extracted
# test_chemical = lapply(out, function(doc){
#   doc$studies %>% 
#     select(test_substance_name_original, 
#            test_substance_name_secondary_original, 
#            test_substance_casrn_original) %>%
#     mutate(doc_id = doc$documents$id)
# }) %>% dplyr::bind_rows() %>%
#   distinct()
# View(test_chemical)
# 
# tmp_data$Studies %>% 
#   select(test_substance_name, test_substance_name_secondary, test_substance_casrn) %>% 
#   distinct() %>%
#   View()
# 
# # Compare analyte chemical extracted
# analyte_chemical = lapply(out, function(doc){
#   doc$series %>% 
#     select(analyte_name_original, 
#            analyte_name_secondary_original, 
#            analyte_casrn_original) %>%
#     mutate(doc_id = doc$documents$id)
# }) %>% dplyr::bind_rows() %>%
#   distinct()
# View(analyte_chemical)
# 
# tmp_data$Series %>% 
#   select(analyte_name, analyte_name_secondary, analyte_casrn) %>% 
#   distinct() %>%
#   View()
# 
# # Compare all Studies, Subjects, Series, and Conc_Time_Values
# View(tmp_data$Series)
# # View(out$`143`$series)
# View(tmp_data$Studies)
# # View(out$`145`$studies)
# View(tmp_data$Subjects)
# View(tmp_data$Conc_Time_Values %>% select(time, conc))
# # View(out$`147`$conc_time_values %>% select(time_original, time_hr, conc_original))
# 
# comp1 = tmp_data$Conc_Time_Values %>%
#   tidyr::unite("comp", time, conc) %>%
#   select(comp)
# comp2 = out$`145`$conc_time_values %>%  
#   tidyr::unite("comp1", time_original, conc_original, remove=FALSE) %>%
#   tidyr::unite("comp2", time_hr, conc_original) %>%
#   select(comp1, comp2)
# 
# length(comp1$comp[comp1$comp %in% comp2$comp1]) / nrow(comp2) * 100
# length(comp1$comp[comp1$comp %in% comp2$comp2]) / nrow(comp2) * 100
