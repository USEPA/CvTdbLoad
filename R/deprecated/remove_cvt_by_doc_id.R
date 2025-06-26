#' @title remove_cvt_by_doc_id
#' @description FUNCTION_DESCRIPTION
#' @param doc_id String or numeric document identifier value for a record in the database Documents table.
#' @return None. SQL statements are run to delete a document and all linked foreign key records.
#' @rdname remove_cvt_by_doc_id
#' @export 
remove_cvt_by_doc_id <- function(doc_id){
  # Check input doc_id value
  if(is.null(doc_id) || is.na(doc_id)) stop("Input 'doc_id' must not be NULL or NA.")
  if(!is.numeric(doc_id) & !is.character(doc_id)) stop("Input 'doc_id' must be a numeric or character ID value.")
  if(length(doc_id) > 1) stop("Input'doc_id' must be a single ID value.")
  
  # Check if doc_id exists
  doc_id_test = db_query_cvt(paste0("SELECT * FROM cvt.documents WHERE id = ", doc_id))
  if(!nrow(doc_id_test)){
    message("No document of ID '", doc_id, "' present in documents table...returning...")
    return()
  }
  
  # Pull relevant ID values based on Extraction Document ID
  study_ids = db_query_cvt(paste0("SELECT distinct id FROM cvt.studies WHERE fk_extraction_document_id = ", doc_id))
  series_subject_ids = db_query_cvt(paste0("SELECT distinct id, fk_subject_id FROM cvt.series where fk_study_id in (", 
                                    toString(study_ids$id), 
                                    ")"))
  conc_ids = db_query_cvt(paste0("SELECT distinct id FROM cvt.conc_time_values WHERE fk_series_id in (", 
                                 toString(unique(series_subject_ids$id)), 
                                 ")"))
  # Perform Deletions
  message("Are you sure you wish to delete all data associated with extraction document '", doc_id, "'?")
  browser()
  message("Performing deletions...")
  # Delete conc_time_values
  db_query_cvt(paste0("DELETE FROM cvt.conc_time_values WHERE id in (", 
                      toString(unique(conc_ids$id)),
                      ")"))
  # Delete series
  db_query_cvt(paste0("DELETE FROM cvt.series WHERE id in (", 
                      toString(unique(series_subject_ids$id)),
                      ")"))
  # Delete subjects
  db_query_cvt(paste0("DELETE FROM cvt.subjects WHERE id in (", 
                      toString(unique(series_subject_ids$fk_subject_id)),
                      ")"))
  # Delete studies
  db_query_cvt(paste0("DELETE FROM cvt.studies WHERE id in (", 
                      toString(unique(study_ids$id)),
                      ")"))
  # Delete document
  db_query_cvt(paste0("DELETE FROM cvt.documents WHERE id = ", doc_id))
  message("Done.")
}
