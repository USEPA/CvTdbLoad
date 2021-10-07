
#'@description Helper function to get the concentration medium dictionary form the CvT database
get_conc_medium_dict <- function(){
  return(query_cvt("SELECT * FROM conc_medium_dict"))
}

get_unique_conc_medium <- function(fileList, template_path){
  #Get administration route from files
  cm = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Series %>% select(conc_medium) %>% unique() %>% unlist() %>% unname()
  }) %>% 
    unlist()
  #Prep for matching
  out = data.frame(conc_medium_original = cm) %>%
    mutate(conc_medium_original = trimws(tolower(conc_medium_original))) %>%
    unique() %>%
    #Attempt match
    left_join(get_conc_medium_dict(), by="conc_medium_original") %>%
    filter(is.na(conc_medium_normalized))
  
  #Output to file for curation
  writexl::write_xlsx(out %>% select(-id), paste0("input/conc_medium/conc_medium_to_curate_",Sys.Date(),".xlsx"))
}

create_conc_medium_dict <- function(overwrite = FALSE){
  if(overwrite){
    tmp = query_cvt("SELECT DISTINCT conc_medium_original, conc_medium_normalized FROM series") %>%
      mutate(id := NA, .before=conc_medium_original,
             conc_medium_original = trimws(tolower(conc_medium_original)))
    
    push_tbl_to_db(dat=tmp,
                   tblName="conc_medium_dict",
                   overwrite=TRUE,
                   fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                  conc_medium_original="varchar(100)",
                                  conc_medium_normalized="varchar(100)"
                   ))  
  } else {
    message("...Set overwrite to 'TRUE' to overwrite existing concentration medium dictionary")
  }
}
