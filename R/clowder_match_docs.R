#' @description Function to help match template documents to Clowder files
#' @param df Input template document's sheet for mapping
#' @param dsID Clowder dataset ID to pull from.
#' @param apiKey API key to access Clowder repo
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [left_join][dplyr::left_join], [arrange][dplyr::arrange]
#' @rdname clowder_match_docs
#' @export 
#' @importFrom dplyr filter mutate left_join arrange
clowder_match_docs <- function(df=NULL, dsID=NULL, baseurl=NULL, apiKey=NULL){
  message("Matching clowder documents to template identifiers...")
  if(is.null(apiKey)) stop("Error: missing required Clowder apiKey")
  if(is.null(dsID)) stop("Error: missing required Clowder dataset ID")
  if(is.null(baseurl)) stop("Error: missing required Clowder URL")
  
  if(!"pdf_filepath" %in% names(df)){ df$pdf_filepath = as.character(NA) }
  # Attempt to match to PMID
  # c_docs = get_clowder_docList_2(dsID=dsID, baseurl=baseurl, apiKey=apiKey)
  c_docs = clowder_get_dataset_files(dsID=dsID, baseurl=baseurl, apiKey=apiKey) %>%
    dplyr::select(-folders.name) %>%
    dplyr::distinct() %>%
    dplyr::rename(clowder_file_id = clowder_id) %>%
    dplyr::group_by(filename) %>%
    # Collapse duplicates
    dplyr::mutate(clowder_file_id = paste0(clowder_file_id, collapse = "; ")) %>%
    dplyr::distinct()
    
  pmid_match = df %>%
    dplyr::filter(!is.na(pmid)) %>%
    dplyr::mutate(pdf_filepath = paste0("PMID", pmid, ".pdf")) %>%
    #Match to Clowder filename
    dplyr::left_join(c_docs,
              by=c("pdf_filepath"="filename")) %>%
    # Filter to those that matched
    dplyr::filter(!is.na(clowder_file_id))
  df = df %>%
    dplyr::filter(!id %in% pmid_match$id)
  
  other_match = df %>%
    dplyr::filter(!is.na(other_study_identifier)) %>%
    dplyr::mutate(pdf_filepath = paste0(other_study_identifier, ".pdf")) %>%
    # Match to Clowder filename
    dplyr::left_join(c_docs,
              by=c("pdf_filepath"="filename")) %>%
    # Filter to those that matched
    dplyr::filter(!is.na(clowder_file_id))
  
  df = df %>%
    dplyr::filter(!id %in% other_match$id) %>%
    dplyr::mutate(clowder_file_id = as.character(NA),
                  folders.name = as.character(NA))
  
  # Recombine and return
  dplyr::bind_rows(df, pmid_match, other_match) %>%
    dplyr::arrange(id) %>%
    return()
}
