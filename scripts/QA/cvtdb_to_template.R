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
#' @param template_map Template field name map
#' @param include_foreign_keys Boolean whether to return template with database foreign keys. Default is FALSE.
cvtdb_to_template <- function(id=NULL, template_path=NULL, template_map=NULL, include_foreign_keys=FALSE){
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
  # template_map = "input/qa_template_map.xlsx"
  map = readxl::read_xlsx(template_map) %>%
    # Update map based on database - add missing map entries
    update_field_map()
  
  # Process pulled data into template format and return
  convert_cvt_to_template(in_dat = cvt_data, 
                          template = cvt_template, 
                          map = map, 
                          include_foreign_keys = include_foreign_keys) %>%
    return()
}

#' @description Function to update field map with missing database fields by table
update_field_map <- function(in_map = NULL){
  
  db_col_list = db_query_cvt(paste0("SELECT table_name AS sheet, column_name AS from ",
                                    "FROM information_schema.columns ",
                                    "WHERE table_schema = 'cvt' ",
                                    "AND table_name in ('",
                                    paste0(unique(in_map$sheet), collapse="', '"),
                                    "')"))
  
  for(s in unique(in_map$sheet)){
    missing_f = db_col_list %>%
      dplyr::filter(sheet == s,
                    !from %in% in_map$from[in_map$sheet == s]) %>%
      dplyr::mutate(to = from)
    
    if(nrow(missing_f)){
      in_map = in_map %>%
        dplyr::bind_rows(missing_f)  
    }
  }
  
  return(in_map)
  
}

get_cvt_by_doc_id <- function(id){
  doc_filter = lapply(names(id), function(i){
    if(is.null(id[[i]])) return(NULL)
    if(is.na(id[[i]])) return(NULL)
    paste0(i, " in ('", paste0(id[[i]], collapse= "', '"), "')")
  }) %>% purrr::compact()
  
  cat("...getting document data...\n")
  doc_data = db_query_cvt(paste0("SELECT * FROM cvt.documents WHERE ", 
                                 paste0(doc_filter, 
                                        collapse = " AND ")) %>% 
                         stringr::str_squish()) %>%
    dplyr::mutate(document_type = "1")
  # Check if any records matched the input identifiers
  if(!nrow(doc_data)) return(NULL)
  cat("...getting document lineage data...\n")
  doc_lineage = db_query_cvt(paste0("SELECT a.*, b.relationship_type as document_type FROM cvt.documents a ",
                                    "LEFT JOIN cvt.documents_lineage b ON a.id = b.fk_doc_id ",
                                    "WHERE a.id IN (", 
                                    "SELECT fk_doc_id FROM cvt.documents_lineage WHERE fk_parent_doc_id IN (",
                                    toString(doc_data$id), ")", 
                                    ")"))
  
  if(nrow(doc_lineage)){
    doc_data = doc_data %>%
      dplyr::bind_rows(doc_lineage %>%
                         # Translate document_type back to numerics from SOP
                         dplyr::mutate(document_type = dplyr::case_when(
                           document_type == "Reference Document" ~ "2",
                           document_type == "Supplemental Document" ~ "3",
                           document_type == "Study Methods Document" ~ "4",
                           TRUE ~ document_type
                         ))) %>%
      dplyr::distinct()
  }

  cat("...getting study data...\n")
  study_data = db_query_cvt(paste0("SELECT a.*, ",
                                   "b.administration_form_original, b.administration_form_normalized, ",
                                   "c.administration_method_original, c.administration_method_normalized, ",
                                   "d.administration_route_original, d.administration_route_normalized, ",
                                   "e.dose_frequency_original, e.dose_frequency_normalized ",
                                   "FROM cvt.studies a ",
                                   "LEFT JOIN cvt.administration_form_dict b ON a.fk_administration_form_id = b.id ",
                                   "LEFT JOIN cvt.administration_method_dict c ON a.fk_administration_method_id = c.id ",
                                   "LEFT JOIN cvt.administration_route_dict d ON a.fk_administration_route_id = d.id ",
                                   "LEFT JOIN cvt.dose_frequency_dict e ON a.fk_dose_frequency_id = e.id ",
                                   "WHERE a.fk_extraction_document_id in (", 
                                   toString(doc_data$id), 
                                   ")"))
  # Case where no study data is linked to extraction document
  if(!nrow(study_data)){
    list(Documents = doc_data,
         Studies = NULL,
         Subjects = NULL,
         Series = NULL,
         Conc_Time_Values = NULL) %>%
      return()
  }
  
  ref_doc_ids = unique(study_data$fk_reference_document_id[!is.na(study_data$fk_reference_document_id)]) %>%
    .[!. %in% doc_data$id]
  
  # Pull additional ref documents not in documents_lineage (in case there are orphaned connections)
  if(length(ref_doc_ids)){
    cat("...getting reference document data...\n")
    ref_doc_data = db_query_cvt(paste0("SELECT * FROM cvt.documents WHERE id in (", toString(ref_doc_ids), ")"))  
  } else {
    ref_doc_data = NULL
  }
  
  cat("...getting series data...\n")
  series_data = db_query_cvt(paste0("SELECT a.*, ",
                                    "b.conc_medium_original, b.conc_medium_normalized ",
                                    "FROM cvt.series a ",
                                    "LEFT JOIN cvt.conc_medium_dict b ON a.fk_conc_medium_id = b.id ",
                                    "WHERE fk_study_id in (", 
                                    toString(study_data$id), 
                                    ")"))
  cat("...getting subject data...\n")
  subject_data = db_query_cvt(paste0("SELECT * FROM cvt.subjects WHERE id in (", toString(unique(series_data$fk_subject_id)), ")"))
  cat("...getting conc data...\n")
  conc_data = db_query_cvt(paste0("SELECT * FROM cvt.conc_time_values WHERE fk_series_id in (", toString(series_data$id), ")"))
  cat("...returning...\n")
  list(Documents = doc_data %>%
         dplyr::bind_rows(ref_doc_data) %>% 
         dplyr::distinct(),
       Studies = study_data,
       Subjects = subject_data,
       Series = series_data,
       Conc_Time_Values = conc_data) %>%
    return()
}

convert_cvt_to_template <- function(in_dat=NULL, template=NULL, map=NULL, include_foreign_keys=NULL){
  # Map field names to template
  in_dat = lapply(names(in_dat), function(s){
    # If include foreign_keys, add them to the template and map
    if(include_foreign_keys){
      # Add to template
      fk_list = names(in_dat[[s]])[grepl("^fk_", names(in_dat[[s]]))] 
      fk_list = fk_list[!fk_list %in% names(template[[s]])]
      template[[s]] = template[[s]] %>%
        cbind(
          data.frame(matrix(ncol=length(fk_list),nrow=0, dimnames=list(NULL, fk_list)))
        )
      # Add to map
      fk_list = names(in_dat[[s]])[grepl("^fk_", names(in_dat[[s]]))] 
      fk_list = fk_list[!fk_list %in% map$from]
      map = map %>%
        dplyr::bind_rows(
          data.frame(from=fk_list, to=fk_list) %>%
            dplyr::mutate(sheet=s)
        )
        
    }
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
    } else if(s == "Conc_Time_Values"){
      # Sort by fk_series and time values
      tmp = tmp %>%
        arrange(fk_series_id, time)
    }
    return(tmp)
  }) %T>% {
    names(.) <- names(in_dat)
  } %>% 
    return()
}
