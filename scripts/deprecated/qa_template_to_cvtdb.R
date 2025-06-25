# Convert CVTDB data back into template format
# By: Jonathan Taylor Wall
# Created: 2023-2-27
# R version 4.1.2 (2021-11-01)
# jsonlite_1.7.3; purrr_0.3.4; tidyr_1.1.4; magrittr_2.0.1; dplyr_1.0.7
# readr_2.1.2; writexl_1.4.0

#' qa_template_to_cvtdb
#' Uses other package helper functions to prepare QC Complete template data to
#' push back into CVTDB using the developed database audit triggers.
#' document ID information, and generates the template
#' @param in_file Filepath to a template to load.
#' @param template_path File path to latest template.
qa_template_to_cvtdb <- function(in_file, template_path){
  # Check parameters
  if(is.null(template_path)) stop("Must provide a 'template_path' so data may be formatted into it.")
  if(is.null(template_map)) stop("Must provide a 'template_map' so data field names may be mapped as needed.")
  # Load empty template to populate
  # template_path = "input/qc_template.xlsx"
  # cvt_template = get_cvt_template(template_path)
  # Template field map
  # template_map = "input/template_map.xlsx"
  map = readxl::read_xlsx(template_map)
  
  # Load template
  in_dat = load_sheet_group(fileName = in_file, template_path = template_path)
  
  ##############################
  ### Run normalization routines
  ##############################
  #Create/clear log entry for filename
  log_CvT_doc_load(in_file, m=NULL, reset=TRUE)
  
  in_dat$Subjects$species = normalize_species(x=in_dat$Subjects$species)
  
  in_dat$Studies = in_dat$Studies %>%
    dplyr::rename(administration_route_original = administration_route) %>%
    mutate(administration_route_original = tolower(administration_route_original)) %>%
    left_join(readxl::read_xlsx("input\\dictionaries\\administration_route_dict.xlsx") %>%
                dplyr::rename(fk_administration_route = id),
              by="administration_route_original")
  
  in_dat = normalize_CvT_data(df=in_dat, f=in_file)
  # Check if normalized data has all required fields (and no NA missing values in required fields)
  check_required_fields(df=in_dat, f=in_file)
  
  # Translate field names to CvTdb names
  in_dat = lapply(names(in_dat), function(s){
    message("Working on sheet: ", s)
    tmp = in_dat[[s]] %T>% {
      # Have to map CvT database names back to the template (usually a _original stem)
      message("...Renaming mapped variables...", Sys.time())
      names(.)[names(.) %in% map$to[map$sheet == tolower(s)]] <- left_join(data.frame(to=names(.)[names(.) %in% 
                                                                                                    map$to[map$sheet == tolower(s)]], 
                                                                                      stringsAsFactors = F), 
                                                                           map[map$sheet==tolower(s),], 
                                                                           by = "to") %>% 
        select(from) %>% 
        mutate(from = as.character(from)) %>% 
        unlist()
      message("...Returning converted data...", Sys.time())
    }
  }) %T>% {
    names(.) <- names(in_dat)
  }

  # Process QC Fields
  in_dat$Documents = in_dat$Documents %>%
    dplyr::rename(created_by = qc_reviewer_lanid)
  
  # Check field name alignment with database
  name_check = lapply(names(in_dat), function(tbl_name){
    db_names = db_query_cvt(paste0("SELECT * FROM cvt.", tbl_name, " LIMIT 1")) %>% names()
    qc_names = names(in_dat[[tbl_name]])
    
    return(qc_names[!qc_names %in% db_names])
  }) %T>% {
    names(.) <- names(in_dat)
  } %>% unlist()
  
  # TODO Reconcile additional fields (e.g., fix Series normalization having updated_by and rec_update_dt fields)
  if(length(name_check)){
    browser()
    stop("Need to update database and/or reconcile additional fields found...")
  }
  
  # Cache normalized template
  # save_normalized_template(df=in_dat, f=f)
}