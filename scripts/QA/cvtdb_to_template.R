# Convert CVTDB data back into template format
# By: Jonathan Taylor Wall
# Created: 2023-1-6
# R version 4.1.2 (2021-11-01)
# jsonlite_1.7.3; purrr_0.3.4; tidyr_1.1.4; magrittr_2.0.1; dplyr_1.0.7
# readr_2.1.2; writexl_1.4.0

#' cvtdb_to_template
#' Uses other package helper functions to pull data from CVTDB, filters by input
#' document ID information, and generates the template
#' @param id A character or list of document ID information to filter by
#' @param template_path File path to latest template
cvtdb_to_template <- function(id=NULL, template_path=NULL){
  # Load empty template to populate
  cvt_template = get_cvt_template()  
  
  query_cvt()
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
