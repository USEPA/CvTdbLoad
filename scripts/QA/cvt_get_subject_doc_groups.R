#' @title cvt_get_subject_doc_groups
#' @description Identify cases where the same subject ID is shared across documents. This should not happen.
#' @return Dataframe of candidate records to review.
#' @seealso 
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{filter}}
#' @rdname cvt_get_subject_doc_groups
#' @export 
#' @importFrom dplyr select distinct group_by mutate across any_of na_if filter
cvt_get_subject_doc_groups <- function(){
  in_data = db_query_cvt(paste0("select distinct ",
                                "b.fk_extraction_document_id, ",
                                "b.fk_reference_document_id, ", 
                                "b.id as study_id, a.fk_subject_id, a.id as series_id ",
                                "from cvt.series a ",
                                "left join cvt.studies b ",
                                "on a.fk_study_id = b.id"))
  
  # Collapse groups to get document and curation tag lists
  shared_subs = in_data %>%
    dplyr::select(fk_extraction_document_id, fk_subject_id) %>%
    dplyr::distinct() %>%
    dplyr::group_by(fk_subject_id) %>%
    # dplyr::summarise(n = n())
    dplyr::mutate(dplyr::across(dplyr::any_of(names(in_data)[!names(in_data) %in% c("fk_subject_id")]),
                                ~paste0(sort(unique(.[!is.na(.)])),
                                        collapse = ", ") %>%
                                  dplyr::na_if("NA") %>%
                                  dplyr::na_if("") %>%
                                  dplyr::na_if("-")
    )) %>%
    # dplyr::mutate(id = paste0(sort(unique(id[!is.na(id)])), collapse=", ")) %>%
    dplyr::distinct() %>%
    dplyr::filter(grepl(",", fk_extraction_document_id))
  
  # Get list of affected documents
  shared_subs$fk_extraction_document_id %>% paste0(., collapse = ", ") %>% strsplit(", ") %>% unlist() %>% unique()
  
  return(shared_subs)
}
