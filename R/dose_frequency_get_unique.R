dose_frequency_get_unique <- function(fileList, template_path){
  #Get administration route from files
  cm = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Studies %>% dplyr::select(dose_frequency) %>% unique() %>% unlist() %>% unname()
  }) %>% 
    unlist()
  #Prep for matching
  out = data.frame(dose_frequency_original = cm) %>%
    dplyr::mutate(dose_frequency_original = trimws(tolower(dose_frequency_original))) %>%
    unique() %>%
    #Attempt match
    #dplyr::left_join(dose_frequency_get_dict(), by="dose_frequency_original") %>%
    dplyr::filter(is.na(conc_medium_normalized))
  
  #Output to file for curation
  writexl::write_xlsx(out %>% dplyr::select(-id), paste0("input/dose frequency/dose_frequency_to_curate_",Sys.Date(),".xlsx"))
}
