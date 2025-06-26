#' @title Check foreign keys validator
#' @description Function to check if processed document's foreign keys match to a value in their respective sheets.
#' @param df List of dataframes for the sheets within an extraction template
#' @param f Filename for flagging purposes
#' @param log_path File path where to save the log file. Default "output/template_normalization_log.xlsx"
#' @return None. Logs any flags
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [validator][validate::validator]
#' @rdname validate_foreign_keys
#' @export 
validate_foreign_keys <- function(df, f, log_path, verbose=FALSE){
  validation <- TRUE # False if an invalid condition was encountered

  # Skip case of only having documents sheet
  if(length(names(df)) == 1 && names(df) == c("Documents")){
    return (validation)
  }
  
  # Get present id's within relevant sheets
  study_ids <- df$Studies$id[!is.na(df$Studies$id)]
  subject_ids <- df$Subjects$id[!is.na(df$Subjects$id)]
  series_ids <- df$Series$id[!is.na(df$Series$id)]
  
  # Loop through each sheet
  for (sheet in c("Series", "Conc_Time_Values")) {
    # If the document is a qc_document, and it has a fail status, no need to validate
    if("qc_status" %in% names(df[[sheet]])) {
      df[[sheet]] <- df[[sheet]] %>% dplyr::filter(!qc_status %in% "fail")
    }
    
    # Pull rules from the respective YAML, and store failing validation checks
    rules <- validate::validator(.file=paste0("input/rules/foreign_keys/", sheet,".yaml"))
    out <- validate::confront(df[[sheet]], rules, ref=list(study_ids=study_ids, subject_ids=subject_ids, series_ids=series_ids))
    fails <- validate::summary(out) %>% 
      dplyr::filter(fails > 0)
    
    # Loop through each failure and log the message
    for (i in seq_len(nrow(fails))) {
      m <- validate::meta(rules[fails$name[i]])$message
      message(paste0(sheet, ": ", m))
      log_CvT_doc_load(f=f, m=m, log_path=log_path)
      validation <- FALSE
    }

    # If the verbose parameter is enabled, print extra information about the failing entries
    if (verbose && nrow(fails) > 0) {
      print(validate::violating(df[[sheet]], out))
    }
  }

  return (validation)
}
