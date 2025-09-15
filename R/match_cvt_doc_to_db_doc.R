#' @title match_cvt_doc_to_db_doc
#' @description Function to match input document metadata to CvTdb document records by pmid, other_study_identifier, doi, url hierarchy.
#' @param df Input dataframe of Document records to try to match to database Documents table entries, Default: NULL.
#' @return Modified input `df` dataframe with matched database Documents table ID values.
#' @seealso 
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{tidyeval-compat}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname match_cvt_doc_to_db_doc
#' @export 
#' @importFrom dplyr select sym filter distinct pull mutate across any_of left_join bind_rows arrange
#' @importFrom stringr str_squish
match_cvt_doc_to_db_doc <- function(df=NULL){
  orig_doc_count = nrow(df)
  
  check_list = c("pmid", "other_study_identifier", "doi", "url", "title")
  # Find potential duplicate values per ID level (irrespective of the hierarchy)
  where_clause = lapply(check_list, function(x){
    tmp = df %>%
      dplyr::select(!!dplyr::sym(x)) %>%
      dplyr::filter(!is.na(!!dplyr::sym(x))) %>% 
      dplyr::distinct() %>%
      dplyr::pull() %>%
      stringr::str_squish() %>%
      unique()
  }) %T>% { names(.) <- check_list }
  # Pull all document data
  # input = db_query_cvt("SELECT * FROM cvt.documents")
  # Loop through the hierarchy to verify potenital duplicates
  doc_list = list()
  # Set levels as character
  df = df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(check_list), ~as.character(.)))
  
  for(level in check_list){# Check each level, then filter out matched and to those missing a level entry
    if(!length(where_clause[[level]])){
      # No level filter found, skip
      next
    }
    tmp = db_query_cvt(paste0("SELECT id as fk_document_id, ", level," FROM cvt.documents where ", 
                              level, " in ('", where_clause[[level]] %>%
                                # Escape single quotation prime symbol with double
                                # so SQL query works
                                gsub("'", "''", .) %>%
                                paste0(collapse="', '"),
                              "')")) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(level), ~as.character(.)))
    
    doc_list[[level]] = df %>%
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish)) %>%
      dplyr::filter(!is.na(!!dplyr::sym(level))) %>%
      dplyr::left_join(tmp, by=level)
    df = df %>% # Filter out those with matches found
      dplyr::filter(!id %in% doc_list[[level]]$id)
  }
  
  doc_list = doc_list %>%
    dplyr::bind_rows() %>% 
    dplyr::arrange(id)
  
  # Those without matches get fk_document_id = NA
  df$fk_document_id = NA 
  # Recombine and return to user
  out = doc_list %>%
    dplyr::bind_rows(df) %>%
    dplyr::arrange(id)
    
  if(nrow(out) == orig_doc_count){
    return(out)
  } else {
    stop("Multiple doc matches found...")
  }
}
