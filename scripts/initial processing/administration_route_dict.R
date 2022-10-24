
#'@description Helper function to get the administration route dictionary form the CvT database
get_administration_route_dict <- function(){
  return(query_cvt("SELECT * FROM cvt.administration_route_dict"))
}

get_unique_administration_route <- function(fileList, template_path){
  #Get administration route from files
  ar = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Studies %>% 
      select(administration_route_original = administration_route) %>% 
      unique() %>%
      mutate(filepath = f)
  }) %>% 
    dplyr::bind_rows()
  #Prep for matching
  out = ar %>%
    mutate(administration_route_original = trimws(tolower(administration_route_original))) %>%
    distinct() %>%
    #Attempt match
    left_join(get_administration_route_dict(), by="administration_route_original") %>%
    filter(is.na(administration_route_normalized)) %>%
    select(filepath, administration_route_original, administration_route_normalized)
  
  #Output to file for curation
  if(nrow(out)){
    writexl::write_xlsx(out, paste0("input/administration_route/administration_route_to_curate_",Sys.Date(),".xlsx"))
  } else {
    message("...No new administration routes to curate...returning")  
  }
  return(out)
}

create_administration_route_dict <- function(overwrite = FALSE){
  if(overwrite){
    tmp = query_cvt("SELECT DISTINCT administration_route_original, administration_route_normalized FROM cvt.studies") %>%
      mutate(id := NA, .before=administration_route_original,
             administration_route_original = trimws(tolower(administration_route_original)))
    
    push_tbl_to_db(dat=tmp,
                   tblName="administration_route_dict",
                   overwrite=TRUE,
                   fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                  administration_route_original="varchar(100)",
                                  administration_route_normalized="varchar(100)"
                   ))  
  } else {
    message("...Set overwrite to 'TRUE' to overwrite existing administration route dictionary")
  }
}

update_administration_route_dict <- function(dict_file){
  if(file.exists(dict_file)){
    #Filter to only new entries to append to dictionary
    new = readxl::read_xlsx(dict_file) %>%
      select(-id) %>%
      mutate(administration_route_original = trimws(tolower(administration_route_original))) %>%
      filter(!is.na(administration_route_normalized), administration_route_normalized != "NA") %>%
      anti_join(get_administration_route_dict() %>% 
                  select(administration_route_original, administration_route_normalized) %>%
                  mutate(administration_route_original = trimws(tolower(administration_route_original))))
    #Push new dictionary entries
    push_tbl_to_db(dat=new,
                  tblName="administration_route_dict",
                  overwrite=FALSE, append=TRUE)
  } else {
    message("...input dict_file does not exist...cannot push dictionary")
  }
}