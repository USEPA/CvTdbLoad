# Orchestration script for pulling CvTdb data into template format
# Jonathan Taylor Wall
# Modified 2023-6-2
# R version 4.1.2 (2021-11-01)

# id_list = c(32, 60, 14, 1, 36, 18, 38, 59, 46, 45, 51, 129, 130, 132, 133, 135, 131, 158, 159, 160, 161, 162, 164, 157, 170, 159, 137, 175, 141, 6, 24785, 24983)
# id_list = c(1, 129, 130, 132, 133, 131, 135, 6, 137, 134, 139, 141, 14, 18, 149, 150, 24983, 151, 153, 157, 158, 159, 32, 160, 161, 162, 36, 164, 38, 165, 166, 167, 170, 169, 171, 45, 46, 175, 172, 173, 174, 51, 59, 60, 192, 24785)

# id_list = c(11, 14, 17, 22, 30, 31, 46, 48, 50, 51, 52, 62, 63, 64, 128, 192, 244)

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
  if(is.null(names(id))) stop(paste0("Must provide document id information as named list of options: ", toString(allowed_doc_id)))
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
