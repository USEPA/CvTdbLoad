# Script to automate extraction for NTP style CvT data documents
# Created by: Jonathan Taylor Wall
# Created Date: 2023-03-8
#Load packages
# require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl); library(httk)
# R version 4.1.0 (2021-05-18)
# httk_2.1.0; readxl_1.3.1; tidyr_1.2.0; magrittr_2.0.2; dplyr_1.0.8; DBI_1.1.2
# purrr_0.3.4; assertthat_0.2.1
#Load R Scripts

#' extract_ntp_data_file
#' @description Function to semi-automate extraction of NTP data to CvT template
#' @param filepath File path to the NTP file to extract to CvT template
#' @param template_map File path to a field name map from NTP to CvT template names
extract_ntp_data_file <- function(filepath, template_map){
  if(!is.null(filepath) & is.na(filepath)) return(NULL)
  if(!file.exists(filepath)) return(NULL)
  
  # Get sheet list (species - aministration_route)
  s_list = readxl::excel_sheets(filepath)
  
  # Pull data from all sheets
  in_dat = lapply(s_list, function(s){
    tmp = readxl::read_xlsx(filepath, sheet=s)
    if(s != "Intro"){
      # Filter fields when not Intro sheet
      tmp %>%
        filter(!is.na(Species)) %>%
        mutate(sheet_name = s) 
    }
     return(tmp)
  }) %T>% {
    names(.) <- s_list
  }
  
  # Get intro data separated and transformed into usable format
  intro_dat = in_dat %>% 
    purrr::pluck("Intro") %>%
    rbind(paste("Title:", names(.))) %T>% {
      names(.) <- "intro_metadata"
    } %>%
    filter(!is.na(intro_metadata)) %>%
    tidyr::separate(intro_metadata, into=c("field_name", "value"), sep=": ")
  # Remove intro data sheet, then combine
  in_dat = in_dat %>%
    purrr::list_modify("Intro" = NULL) %>%
    dplyr::bind_rows()
  
  # Pull template to map fields
  # template_path = "L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/CvT_data_template_articles.xlsx"
  template = get_cvt_template(template_path = template_path)
  # template_map = "input/ntp_template_map.xlsx"
  map = readxl::read_xlsx(template_map)
  
  # Loop through the template and populate the fields
  # Use field map to select and populate
  # Map field names to template
  out = lapply(names(template), function(s){
    message("Working on sheet: ", s)
    tmp = in_dat %T>% {
      # Have to map CvT database names back to the template (usually a _original stem)
      message("...Renaming mapped variables...", Sys.time())
      names(.)[names(.) %in% map$from[map$sheet == tolower(s)]] <- left_join(data.frame(from=names(.)[names(.) %in% 
                                                                                                        map$from[map$sheet == tolower(s)]], 
                                                                                        stringsAsFactors = F), 
                                                                             map[map$sheet==tolower(s),], 
                                                                             by = "from") %>% 
        select(to) %>% 
        mutate(to = as.character(to)) %>% 
        unlist()
      message("...Returning converted data...", Sys.time())
    } %>% 
      # Select all template fields that exist already (ensuring clowder_file_id included)
      select(any_of(names(template[[s]])))
    
    # Fill missing template fields (happens when template is updated compared to older uploaded version)
    tmp[names(template[[s]])[!names(template[[s]]) %in% names(tmp)]] <- NA
    
    # Add reviewer LAN ID field
    if(s == "Documents"){
      tmp$title = intro_dat$value[intro_dat$field_name == "Title"]
      tmp$year = intro_dat$value[intro_dat$field_name == "Start Date"]
    }
    return(tmp %>% distinct())
  }) %T>% {
    names(.) <- names(template)
  }
  return(out)
}

#' get_cvt_template
#' Pull the CvT template in a list of empty dataframes
get_cvt_template <- function(template_path){
  s_list = readxl::excel_sheets(template_path)
  lapply(s_list, function(s){
    readxl::read_xlsx(template_path, sheet=s)
  }) %T>% { names(.) <- s_list } %>%
    return()
}