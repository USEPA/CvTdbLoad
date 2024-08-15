validate_qc_fields <- function(df, f, log_path, verbose=FALSE) {
  validation <- TRUE # False if an invalid condition was encountered
  rules <- validate::validator(.file=paste0("input/rules/qc/QC.yaml"))
  
  # Loop through each sheet
  for (sheet_name in names(df)) {
    # Check if not a QC document, or missing columns, or poorly named columns
    if (!all(c("qc_status", "qc_flags") %in% names(df[[sheet_name]])) ) {
      m <- paste0(sheet_name, ": Missing either qc_status or qc_flags")
      message(m)
      log_CvT_doc_load(f=f, m=m, log_path=log_path)
      return (FALSE)
    }
    
    # Pull rules from the respective YAML, and store failing validation checks
    out <- validate::confront(df[[sheet_name]], rules)
    fails <- validate::summary(out) %>% 
      dplyr::filter(fails > 0)
    
    # Loop through each failure and log the message
    for (i in seq_len(nrow(fails))) {
      validation <- FALSE
      m <- validate::meta(rules[fails$name[i]])$message
      message(paste0(sheet_name, ": ", m))
      log_CvT_doc_load(f=f, m=m, log_path=log_path)
    }

    # If the verbose parameter is enabled, print extra information about the failing entries
    if (verbose && nrow(fails) > 0) {
      print(validate::violating(df[[sheet_name]], out))
    }
  }
  
  return (validation)
}