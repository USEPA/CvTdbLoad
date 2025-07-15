#' @title qa_check_duplicate_documents
#' @description Find duplicates in documents table based on ID hierarchy.
#' @return None. XLSX file written to output folder.
#' @seealso 
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[stringr]{str_length}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname qa_check_duplicate_documents
#' @export 
#' @importFrom dplyr filter mutate select bind_rows arrange
#' @importFrom rlang sym
#' @importFrom stringr str_length
#' @importFrom writexl write_xlsx
qa_check_duplicate_documents <- function(){
  #Input list of ID values to check in hierarchical order
  check_list = c("pmid", "other_study_identifier", "doi", "url")
  #Find potential duplicate values per ID level (irrespective of the hierarchy)
  where_clause = lapply(check_list, function(x){
    tmp = db_query_cvt(paste0("SELECT ",x," from cvt.documents")) %>%
      dplyr::filter(!is.na(!!rlang::sym(x))) %>% unlist() %>% unname()
    tmp = tmp[duplicated(tmp)] %>% unique() %>% paste0(collapse="', '")
  }) %T>% { names(.) <- check_list }
  #Pull all document data
  input = db_query_cvt("SELECT * FROM cvt.documents")
  #Loop through the hierarchy to verify potenital duplicates
  doc_list = list()
  for(level in check_list){#Check each level, then filter out matched and to those missing a level entry
    if(!stringr::str_length(where_clause[[level]])){
      #No duplicates identified in search level, so filter out those with such values
      input = input %>% 
        dplyr::filter(!id %in% doc_list[[level]]$id, #Filter out identified duplicates
               is.na(!!rlang::sym(level))) #Filter to missing from this level
      next #Skip empty search
    }
    doc_list[[level]] = db_query_cvt(paste0("SELECT * FROM cvt.documents where ", 
                                            level, " in ('", where_clause[[level]],"')"))
    doc_list[[level]] = input %>%
      dplyr::filter(!!rlang::sym(level) %in% where_clause[[level]]) %>%
      dplyr::mutate(doc_name = paste0(!!rlang::sym(level), "_", level,"_doc_id_", id)) %>%
      dplyr::select(id, !!rlang::sym(level), doc_name)
    input = input %>% 
      dplyr::filter(!id %in% doc_list[[level]]$id, #Filter out identified duplicates
             is.na(!!rlang::sym(level))) #Filter to missing from this level
  }
  
  doc_list = dplyr::bind_rows(doc_list) %>% dplyr::arrange(doc_name)
  
  # out = lapply(seq_len(nrow(doc_list)), function(r){
  #   message("Pulling data for row: ", r, " ID: ", doc_list$id[r])
  #   #Get document sheet
  #   document = db_query_cvt(paste0("SELECT * FROM cvt.documents WHERE id = ", doc_list$id[r])) %>%
  #     select(-ends_with("_by"), -ends_with("_dt"))
  #   #Get associated studies
  #   study = db_query_cvt(paste0("SELECT * from cvt.studies where fk_extraction_document_id = ", doc_list$id[r],
  #                               " OR fk_reference_document_id = ", doc_list$id[r])) %>%
  #     select(-ends_with("_by"), -ends_with("_dt"))
  #   #Return empty if no studies associated wtih document
  #   if(!nrow(study)) { message("...no studies associated with document..."); return("...no studies associated with document...") }
  #   #Get associated series
  #   series = db_query_cvt(paste0("SELECT * from cvt.series where fk_study_id in (", 
  #                                toString(study$id),")")) %>%
  #     select(-ends_with("_by"), -ends_with("_dt"))
  #   
  #   if(!nrow(series)) { message("...no series associated with document..."); return("...no series associated with document...") }
  #   #Get subject data
  #   if(!length(series$fk_subject_id[!is.na(series$fk_subject_id)])) { message("...CURATION ERROR: no subjects associated with series..."); return("...CURATION ERROR: no subjects associated with series...") }
  #   subjects = db_query_cvt(paste0("SELECT * FROM cvt.subjects where id in (",
  #                                  toString(series$fk_subject_id), ")")) %>%
  #     select(-ends_with("_by"), -ends_with("_dt"))
  #   #Get conc data
  #   conc = db_query_cvt(paste0("SELECT * from cvt.conc_time_values where fk_series_id in (", 
  #                              toString(series$id),")")) %>%
  #     select(-ends_with("_by"), -ends_with("_dt"))
  #   
  #   #Return data
  #   tmp = series %>%
  #     left_join(study, by=c("fk_study_id"="id")) %>%
  #     left_join(subjects, by=c("fk_subject_id"="id")) %>%
  #     left_join(conc, by=c("id"="fk_series_id"))
  #   
  # }) %T>% { names(.) <- doc_list$doc_name } 
  
  message("Number of documents without check_list identifiers: ", nrow(input))
  
  writexl::write_xlsx(list(missing_identifier=input, dups=doc_list),
                      paste0("output/duplicate_doc_candidates_", Sys.Date(), ".xlsx"))
  
}
