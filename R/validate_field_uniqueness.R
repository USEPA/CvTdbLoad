#' @title Validate Field Uniqueness
#' @description Function to check if processed document has unique fields where applicable.
#' @param df List of dataframes for the sheets within an extraction template
#' @param f Filename for flagging purposes
#' @param log_path File path where to save the log file. Default "output/template_normalization_log.xlsx"
#' @return Boolean of document validity. Logs any errors
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [validator][validate::validator]
#' @rdname validate_field_uniqueness
#' @export 
validate_field_uniqueness <- function(df, f, log_path, verbose=FALSE){
  validation <- TRUE # False if an invalid condition was encountered
  
  # Loop through each sheet
  for (sheet in names(df)) {
    # If the document is a qc_document, and it has a fail status, no need to validate
    if("qc_status" %in% names(df[[sheet]])) {
      df[[sheet]] <- df[[sheet]] %>% dplyr::filter(qc_status != "fail")
    }
    
    # Pull rules from the respective YAML, and store failing validation checks
    rules <- validate::validator(.file=paste0("input/rules/field_uniqueness/", sheet,".yaml"))
    out <- validate::confront(df[[sheet]], rules)
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