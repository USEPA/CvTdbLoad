

#'@description Helper function to get the administration route dictionary form the CvT database
get_administration_route_dict <- function(){
  return(query_cvt("SELECT * FROM administration_route_dict"))
}

get_unique_administration_route <- function(fileList, template_path){
  #Get administration route from files
  ar = lapply(fileList, function(f){
    message(f)
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Studies %>% select(administration_route) %>% unique() %>% unlist() %>% unname()
  }) %>% 
    unlist()
  #Prep for matching
  out = data.frame(administration_route_original = ar) %>%
    mutate(administration_route_original = trimws(tolower(administration_route_original))) %>%
    unique() %>%
    #Attempt match
    left_join(admin_dict, by="administration_route_original") %>%
    filter(is.na(administration_route_normalized))
  
  #Output to file for curation
  writexl::write_xlsx(out %>% select(-id), paste0("input/administration_route/administration_route_to_curate_",Sys.Date(),".xlsx"))
  return(out %>% select(-id))
}

create_administration_route_dict <- function(overwrite = FALSE){
  if(overwrite){
    tmp = query_cvt("SELECT DISTINCT administration_route_original, administration_route_normalized FROM studies") %>%
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