#' @title cvt_get_audit_version_mismatch
#' @description Check if there are any table records not audited/versioned correctly. 
#' This is a case where the audit table has a record version that is still "live", so the next UPDATE statement audit
#' routine will fail.
#' @return Dataframe of candidate records that need to be handled.
#' @seealso 
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{unite}}
#' @rdname cvt_get_audit_version_mismatch
#' @export 
#' @importFrom dplyr bind_rows group_by summarise n ungroup filter mutate
#' @importFrom tidyr unite
cvt_get_audit_version_mismatch <- function(){
  in_data = db_query_cvt(paste0("select distinct ",
                                "b.fk_extraction_document_id, ",
                                "b.fk_reference_document_id, ", 
                                "b.id as study_id, a.fk_subject_id, a.id as series_id ",
                                "from cvt.series a ",
                                "left join cvt.studies b ",
                                "on a.fk_study_id = b.id"))
  
  prob_data = data.frame()
  
  for(tbl in c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values", "tk_parameters")){
    tmp = db_query_cvt(paste0("SELECT id, version FROM cvt.", tolower(tbl)))
    tmp2 = db_query_cvt(paste0("SELECT fk_table_id as id, version FROM cvt.cvt_audit WHERE fk_table_name = '", tolower(tbl),"'"))
    
    prob_data = tmp %>%
      dplyr::bind_rows(tmp2) %>%
      dplyr::group_by(id, version) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n > 1) %>%
      dplyr::mutate(fk_table_name = !!tbl) %>%
      dplyr::bind_rows(., prob_data)
  }
  
  out = prob_data %>%
    tidyr::unite(col = 'key', id, fk_table_name, sep = "_") %>%
    dplyr::mutate(key = tolower(key))
  
  audit_id = db_query_cvt(paste0("SELECT id, fk_table_id, fk_table_name FROM cvt.cvt_audit")) %>%
    tidyr::unite(col = 'key', fk_table_id, fk_table_name, sep = "_") %>%
    dplyr::filter(key %in% out$key)
  
  # Delete those in audit
  # db_query_cvt(paste0("DELETE FROM cvt.cvt_audit WHERE id in (", toString(audit_id$id), ")"))
  
  return(audit_id)
}
