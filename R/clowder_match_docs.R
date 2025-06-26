#' @title clowder_match_docs
#' @description Function to match template documents to Clowder files.
#' @param df Input template document's sheet for mapping.
#' @param dsID Clowder dataset ID to pull from.
#' @param baseurl Clowder base URL.
#' @param apiKey API key to access Clowder repo.
#' @param clowder_file_list Opitonal input list of Clowder files from clowder_get_dataset_files().
#' @return Modified `df` with clowder_file_id field for matched documents.
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [left_join][dplyr::left_join], [arrange][dplyr::arrange]
#' @rdname clowder_match_docs
#' @export 
#' @importFrom dplyr filter mutate left_join arrange
clowder_match_docs <- function(df=NULL, dsID=NULL, baseurl=NULL, apiKey=NULL, clowder_file_list=NULL){
  message("Matching clowder documents to template identifiers...")
  if(is.null(apiKey)) stop("Error: missing required Clowder apiKey")
  if(is.null(dsID)) stop("Error: missing required Clowder dataset ID")
  if(is.null(baseurl)) stop("Error: missing required Clowder URL")
  
  if(!"pdf_filepath" %in% names(df)){ df$pdf_filepath = as.character(NA) }
  
  # Filter out already matched clowder_file_id
  if("clowder_file_id" %in% names(df)){
    curated_match = df %>%
      dplyr::filter(!is.na(clowder_file_id))
    df = df %>%
      dplyr::filter(!id %in% curated_match$id) %>%
      # Remove since all NA and will be added if matches found
      dplyr::select(-clowder_file_id)
  } else {
    curated_match = data.frame()
  }
  
  # Attempt to match to PMID
  # c_docs = get_clowder_docList_2(dsID=dsID, baseurl=baseurl, apiKey=apiKey)
  if(is.null(clowder_file_list)){
    clowder_file_list = clowder_get_dataset_files(dsID=dsID, baseurl=baseurl, apiKey=apiKey)
  }
  c_docs = clowder_file_list %>%
    dplyr::select(-folders.name) %>%
    dplyr::distinct() %>%
    dplyr::rename(clowder_file_id = clowder_id) %>%
    dplyr::group_by(filename) %>%
    # Collapse duplicates
    dplyr::mutate(clowder_file_id = paste0(clowder_file_id, collapse = "; ")) %>%
    dplyr::distinct()
  
  pmid_match = df %>%
    dplyr::filter(!is.na(pmid))
  if(nrow(pmid_match)){
    pmid_match = pmid_match %>% 
      dplyr::mutate(pdf_filepath = paste0("PMID", pmid, ".pdf")) %>%
      #Match to Clowder filename
      dplyr::left_join(c_docs,
                       by=c("pdf_filepath"="filename")) %>%
      # Filter to those that matched
      dplyr::filter(!is.na(clowder_file_id))
    df = df %>%
      dplyr::filter(!id %in% pmid_match$id)
  } else {
    pmid_match = data.frame()
  }
  
  other_match = df %>%
    dplyr::filter(!is.na(other_study_identifier))
  if(nrow(other_match)){
    other_match = other_match %>%
      dplyr::mutate(pdf_filepath = paste0(other_study_identifier, ".pdf")) %>%
      # Match to Clowder filename
      dplyr::left_join(c_docs,
                       by=c("pdf_filepath"="filename")) %>%
      # Filter to those that matched
      dplyr::filter(!is.na(clowder_file_id))
    df = df %>%
      dplyr::filter(!id %in% other_match$id)
  } else {
    other_match = data.frame()
  }
    
  # Add missing cols for bind_row()
  df = df %>%
    dplyr::mutate(clowder_file_id = as.character(NA),
                  folders.name = as.character(NA))
  
  # Recombine and return
  dplyr::bind_rows(df, curated_match, pmid_match, other_match) %>%
    dplyr::arrange(id) %>%
    return()
}
