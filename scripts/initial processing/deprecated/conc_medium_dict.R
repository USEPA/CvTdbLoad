
#'@description Helper function to get the concentration medium dictionary form the CvT database
conc_medium_get_dict <- function(){
  return(query_cvt("SELECT * FROM cvt.conc_medium_dict"))
}

conc_medium_get_unique <- function(fileList, template_path){
  #Get administration route from files
  cm = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Series %>% 
      select(conc_medium_original = conc_medium) %>% 
      unique() %>%
      mutate(filepath = f)
  }) %>% 
    dplyr::bind_rows()
  #Prep for matching
  out = cm %>%
    mutate(conc_medium_original = trimws(tolower(conc_medium_original))) %>%
    unique() %>%
    #Attempt match
    left_join(get_conc_medium_dict(), by="conc_medium_original") %>%
    filter(is.na(conc_medium_normalized),
           !is.na(conc_medium_original)) %>%
    select(filepath, conc_medium_original, conc_medium_normalized)
  
  #Output to file for curation
  if(nrow(out)){
    writexl::write_xlsx(out, paste0("input/conc_medium/conc_medium_to_curate_",Sys.Date(),".xlsx"))  
  } else {
    message("...No new concentration media to curate...returning...")
  }
  return(out)
}

conc_medium_create_dict <- function(overwrite = FALSE){
  if(overwrite){
    tmp = query_cvt("SELECT DISTINCT conc_medium_original, conc_medium_normalized FROM cvt.series") %>%
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

conc_medium_update_dict <- function(dict_file){
  if(file.exists(dict_file)){
    new = readxl::read_xlsx(dict_file) %>%
      select(-id, -units) %>%
      mutate(conc_medium_original = trimws(tolower(conc_medium_original))) %>%
      filter(!is.na(conc_medium_normalized), conc_medium_normalized != "NA") %>%
      anti_join(get_conc_medium_dict() %>% 
                  select(conc_medium_original, conc_medium_normalized) %>%
                  mutate(conc_medium_original = trimws(tolower(conc_medium_original)))) %>%
      select(conc_medium_original, conc_medium_normalized)
    
    push_tbl_to_db(dat=new,
                   tblName="conc_medium_dict",
                   overwrite=FALSE, append=TRUE)
  } else {
    message("...input dict_file does not exist...cannot push dictionary")
  }
}
