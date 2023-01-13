# Convert CVTDB data back into template format
# By: Jonathan Taylor Wall
# Created: 2023-1-6
# R version 4.1.2 (2021-11-01)
# jsonlite_1.7.3; purrr_0.3.4; tidyr_1.1.4; magrittr_2.0.1; dplyr_1.0.7
# readr_2.1.2; writexl_1.4.0

#' cvtdb_to_template
#' Uses other package helper functions to pull data from CVTDB, filters by input
#' document ID information, and generates the template
#' @param id A named list of document ID information to filter by
#' @param template_path File path to latest template
cvtdb_to_template <- function(id=NULL, template_path=NULL, template_map=NULL){
  # Check parameters
  if(is.null(template_path)) stop("Must provide a 'template_path' so data may be formatted into it.")
  if(is.null(template_map)) stop("Must provide a 'template_map' so data field names may be mapped as needed.")
  # Allowed document ID fields to filter by
  allowed_doc_id = c("id", "pmid", "other_study_identifier")
  if(is.null(id)) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
  if(!is.list(id)) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
  if(is.null(names(id))) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
  unsupported_id = names(id)[!names(id) %in% allowed_doc_id]
  if(length(unsupported_id)) stop(paste0("Unsupported input document id: ", toString(unsupported_id)))
  
  # Pull data
  cvt_data = get_cvt_by_doc_id(id=id)
  if(is.null(cvt_data)) {
    message("No CVTDB data to pull for provided document ID information...")
    return()
  }
  # Load empty template to populate
  # template_path = "L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/CvT_data_template_articles.xlsx"
  cvt_template = get_cvt_template(template_path)
  # Template field map
  # template_map = "input/template_map.xlsx"
  map = readxl::read_xlsx(template_map)
  # Process pulled data into template format and return
  convert_cvt_to_template(in_dat = cvt_data, template = cvt_template, map = map) %>%
    return()
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

get_cvt_by_doc_id <- function(id){
  doc_filter = lapply(names(id), function(i){
    if(is.null(id[[i]])) return(NULL)
    if(is.na(id[[i]])) return(NULL)
    paste0(i, " in ('", paste0(id[[i]], collapse= "', '"), "')")
  }) %>% purrr::compact()
  
  cat("...getting document data...\n")
  doc_data = query_cvt(paste0("SELECT * FROM cvt.documents WHERE ", paste0(doc_filter, collapse = " AND ")) %>% 
                         stringr::str_squish())
  # Check if any records matched the input identifiers
  if(!nrow(doc_data)) return(NULL)
  cat("...getting study data...\n")
  study_data = query_cvt(paste0("SELECT * FROM cvt.studies WHERE fk_extraction_document_id in (", toString(doc_data$id), ")"))
  cat("...getting reference document data...\n")
  if(length(unique(study_data$fk_reference_document_id[!is.na(study_data$fk_reference_document_id)]))){
    ref_doc_data = query_cvt(paste0("SELECT * FROM cvt.documents WHERE id in (", toString(unique(study_data$fk_reference_document_id[!is.na(study_data$fk_reference_document_id)])), ")"))  
  } else {
    ref_doc_data = NULL
  }
  cat("...getting series data...\n")
  series_data = query_cvt(paste0("SELECT * FROM cvt.series WHERE fk_study_id in (", toString(study_data$id), ")"))
  cat("...getting subject data...\n")
  subject_data = query_cvt(paste0("SELECT * FROM cvt.subjects WHERE id in (", toString(unique(series_data$fk_subject_id)), ")"))
  cat("...getting conc data...\n")
  conc_data = query_cvt(paste0("SELECT * FROM cvt.conc_time_values WHERE fk_series_id in (", toString(series_data$id), ")"))
  cat("...returning...\n")
  list(Documents = rbind(doc_data, ref_doc_data),
       Studies = study_data,
       Subjects = subject_data,
       Series = series_data,
       Conc_Time_Values = conc_data) %>%
    return()
}

convert_cvt_to_template <- function(in_dat=NULL, template=NULL, map=NULL){
  # Map field names to template
  in_dat = lapply(names(in_dat), function(s){
    message("Working on sheet: ", s)
    tmp = in_dat[[s]] %T>% {
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
      select(any_of(c("id", names(template[[s]]), "clowder_file_id")))
    # Fill missing template fields (happens when template is updated compared to older uploaded version)
    tmp[names(template[[s]])[!names(template[[s]]) %in% names(tmp)]] <- NA
    # Return converted template sheet in template order (ensuring clowder_file_id included)
    tmp = tmp %>% 
      select(any_of(c("id", names(template[[s]]), "clowder_file_id"))) %>%
      # Add QC fields
      mutate(qc_notes = NA,
             qc_status = NA,
             qc_flags = NA)
    # Add reviewer LAN ID field
    if(s == "Documents"){
      tmp$qc_reviewer_lanid = NA
    }
    return(tmp)
  }) %T>% {
    names(.) <- names(in_dat)
  } %>% 
    return()
}
