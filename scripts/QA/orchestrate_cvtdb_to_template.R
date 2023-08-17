# Orchestration script for pulling CvTdb data into template format
# Jonathan Taylor Wall
# Created 2023-6-2
# Modified 2023-8-15
# R version 4.1.2 (2021-11-01)

#' @title Pull CvTdb as Template
#' @description Pull CvTdb data into CvT template by document identifiers
#' @param id_list Named list of document identifiers to pull
#' @template_path Path to CvT template for format data into. Default "input/CvT_data_template_articles.xlsx"
#' @template_map Path to map file to rename database fields into CvT template fields. Default "input/qa_template_map.xlsx"
#' @example 
#' id_list = list(id=c(52),
#'                pmid=c(3096853, 11504147),
#'                other_study_identifier=c("C99037B"))
#' orchestrate_cvtdb_to_template(id_list=id_list)
orchestrate_cvtdb_to_template <- function(id_list, 
                              template_path="input/CvT_data_template_articles.xlsx", 
                              template_map="input/qa_template_map.xlsx"){
  
  # Check if input id_list is the expected format
  allowed_doc_id = c("id", "pmid", "other_study_identifier")
  if(is.null(id_list)) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
  if(!is.list(id_list)) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
  if(is.null(names(id_list))) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
  unsupported_id = names(id_list)[!names(id_list) %in% allowed_doc_id]
  if(length(unsupported_id)) stop(paste0("Unsupported input document id: ", toString(unsupported_id)))
  
  # Loop through each ID type
  for(id_type in names(id_list)){
    for(id in id_list[[id_type]]){
      id_check = tryCatch({
        db_query_cvt(paste0("SELECT id, pmid, other_study_identifier FROM cvt.documents where ", 
                            id_type, " = '", id, "'"))
        },
        error = function(e){
          return(NULL)
        })
      # Check if error occured with pull
      if(is.null(id_check)){
        message("Error pulling '", id_type, "' with value '", id, "'")
        next
      }
      # Check if any data pulled
      if(!nrow(id_check)){
        message("Error '", id_type, "' with value '", id, "' does not exist")
        next
      }
      # Check if file already generated (note it is date dependent due to date stamp)
      if(file.exists(paste0("output/CVTDB_QC/", 
                            id_check$id, 
                            "_PMID", id_check$pmid,
                            "_otherID_", id_check$other_study_identifier,
                            "_",
                            Sys.Date() %>% gsub("-", "", .),
                            ".xlsx"))){
        next
      }
      message("Converting document ID '", id_type, "' of value ",  id)
      # Pull data based on input ID values
      out = cvtdb_to_template(id=list(id) %T>% { names(.) <- id_type}, 
                              template_path=template_path, 
                              template_map=template_map)
      # Export pulled data to an Excel file named with identifiers and date stamped
      writexl::write_xlsx(out, paste0("output/CVTDB_QC/", 
                                      out$Documents$id, 
                                      "_PMID", out$Documents$pmid,
                                      "_otherID_", out$Documents$other_study_identifier,
                                      "_",
                                      Sys.Date() %>% gsub("-", "", .),
                                      ".xlsx"))
    } 
  }
}

#'@title get_jira_queued_cvt_qc
#'@description Function to pull a report from CVTDB Jira and filter to QC ticketed document ID values.
#'@param auth_token Jira API authentication token.
#'@param jira_project Name of Jira Project. Default CVTDB.
#'@return Vector of CVTDB document ID's already ticketed in Jira
get_jira_queued_cvt_qc <- function(auth_token = NULL, jira_project="CVTDB"){
  # Format headers
  if(!is.null(auth_token)){
    headers <- c(Authorization = paste0("Bearer ", auth_token))
  } else {
    headers <- NULL
  }
  
  # Pull CSV export from Jira
  url = paste0("https://jira.epa.gov/sr/jira.issueviews:searchrequest-csv-all-fields/temp/SearchRequest.csv?jqlQuery=project+%3D+", 
               jira_project)
  
  temp_in <- tempfile()
  in_data_url <- tryCatch({ 
    utils::download.file(url = url, 
                         destfile = temp_in,
                         headers = headers)
    in_data_url <- readr::read_csv(temp_in, 
                                   col_types = readr::cols()) 
  }, error=function(e) {
    message(e)
    return(NULL)
  }, finally = { unlink(temp_in) })
  
  if(is.null(in_data_url)){
    return()
  }
  # Process jira data to get CVT QC Queued Document Information
  out <- in_data_url %>%
    # Filter to QC tickets
    dplyr::filter(`Custom field (Epic Link)` == "CVTDB-75",
                  # Filter to those with document ID values
                  grepl("QC - Document ID", Summary, fixed = TRUE)) %>%
    # Convert to document ID values
    dplyr::mutate(doc_id = Summary %>%
                    gsub("QC - Document ID", "", .) %>%
                    as.numeric()) %>%
    # Get document ID values
    dplyr::select(doc_id)
  
  return(out$doc_id)
}

#'@title get_cvt_qc_queue
#'@description Function to pull CVTDB document entries with data that have not been
#'queued for QC in Jira.
#'@param auth_token Jira API authentication token.
#'@return DataFrame of CVTDB document entries not queued in Jira for QC.
get_cvt_qc_queue <- function(auth_token = NULL){
  # Pull document information that has CvT data
  ## Conc_Time_Values entries with fk_series_id
  ## fk_study_id for those series ID values
  ## fk_extraction_document_id for those study ID values
  docs_with_data <- db_query_cvt(paste0("SELECT * FROM cvt.Documents where id IN ",
                                        "(SELECT DISTINCT fk_extraction_document_id ",
                                        "FROM cvt.Studies WHERE ID IN ",
                                        "(SELECT DISTINCT fk_study_id ",
                                        "FROM cvt.Series where id IN ",
                                        "(SELECT DISTINCT fk_series_id ",
                                        "FROM cvt.Conc_Time_Values WHERE fk_series_id IS NOT NULL)))"))
  # Get already queued QC document IDs
  jira_queued <- get_jira_queued_cvt_qc(auth_token)
  
  # Filter out already queued QC document IDs and return
  docs_with_data %>%
    dplyr::filter(!id %in% jira_queued) %>%
    return()
}
