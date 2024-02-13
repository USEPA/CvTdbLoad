# Function to match input document metadata to CvTdb document records by pmid, other_study_identifier, doi, url hierarchy
match_cvt_doc_to_db_doc <- function(df=NULL){
  check_list = c("pmid", "other_study_identifier", "doi", "url", "title")
  #Find potential duplicate values per ID level (irrespective of the hierarchy)
  where_clause = lapply(check_list, function(x){
    tmp = df %>%
      dplyr::select(!!sym(x)) %>%
      dplyr::filter(!is.na(!!sym(x))) %>% 
      dplyr::distinct() %>%
      dplyr::pull() %>%
      stringr::str_squish() %>%
      unique() %>% paste0(collapse="', '")
  }) %T>% { names(.) <- check_list }
  #Pull all document data
  #input = db_query_cvt("SELECT * FROM cvt.documents")
  #Loop through the hierarchy to verify potenital duplicates
  doc_list = list()
  # Set levels as character
  df = df %>%
    dplyr::mutate(across(any_of(check_list), ~as.character(.)))
  
  for(level in check_list){#Check each level, then filter out matched and to those missing a level entry
    if(!stringr::str_length(where_clause[[level]])){
      #No level filter found, skip
      next
    }
    tmp = db_query_cvt(paste0("SELECT id as fk_document_id, ", level," FROM cvt.documents where ", 
                              level, " in ('", where_clause[[level]],"')")) %>%
      dplyr::mutate(across(any_of(level), ~as.character(.)))
    
    doc_list[[level]] = df %>%
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish)) %>%
      dplyr::filter(!is.na(!!sym(level))) %>%
      dplyr::left_join(tmp, by=level)
    df = df %>% #Filter out those with matches found
      dplyr::filter(!id %in% doc_list[[level]]$id)
  }
  
  doc_list = doc_list %>%
    dplyr::bind_rows() %>% 
    dplyr::arrange(id)
  
  # Those without matches get fk_document_id = NA
  df$fk_document_id = NA 
  # Recombine and return to user
  rbind(doc_list, df) %>% 
    dplyr::arrange(id) %>%
    return()
}