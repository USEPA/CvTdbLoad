# Script of various utility functions to screens/prep CvT data for loading
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)

#' replace_case_with_NA
#' Postprocessing helper function to orchestrate certain predetermined cases and replace
#' them with 'NA' in the database.
#' @param case_list A list of keyword cases to perform. Allows user to target or skip cases.
#' @return None.
replace_case_with_NA <- function(case_list=NULL){
  
  for(case_na in case_list){
    switch(case_na,
           "conc_na"=replace_conc_na())
  }
}

#' replace_conc_na
#' Case where conc_original is 0 AND no_conc_val_type is not NA, set to NA
replace_conc_na <- function(){
  case_dat <- query_cvt("SELECT id, conc_original, conc, no_conc_val_type FROM cvt.conc_time_values where no_conc_val_type is NOT NULL and conc_original = 0")# and conc_original is NOT NULL")
  
  # Insert logic for an "update" statement that bypasses the audit system
}