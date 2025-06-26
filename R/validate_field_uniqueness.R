#' @title validate_field_uniqueness
#' @description Function to check if processed document has unique fields where applicable.
#' @param df Input named list of dataframes.
#' @param f Filename for logging purposes.
#' @param log_path Path to log.
#' @param verbose Boolean of whether to print additional console messages, Default: FALSE.
#' @return Boolean of whether the input template passed the validation.
#' @seealso 
#'  [validator][validate::validator]
#' @rdname validate_field_uniqueness
#' @export 
#' @importFrom dplyr filter
#' @importFrom validate validator confront summary meta violating
validate_field_uniqueness <- function(df, f, log_path, verbose=FALSE){
  validation <- TRUE # False if an invalid condition was encountered
  
  # Loop through each sheet
  for (sheet in names(df)) {
    # If the document is a qc_document, and it has a fail status, no need to validate
    if("qc_status" %in% names(df[[sheet]])) {
      df[[sheet]] <- df[[sheet]] %>% dplyr::filter(!qc_status %in% "fail")
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
