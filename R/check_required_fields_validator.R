#' @title Check required fields validator
#' @description Function to check if processed document is missing required fields.
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
#' @rdname check_required_fields_new
#' @export 
#' @importFrom
check_required_fields_validator <- function(df, f, log_path){
  # Get reference document id's from studies, used for document id check
  ref_ids = df$Studies$fk_reference_document_id
  ref_ids = ref_ids[!is.na(ref_ids)]
  
  # Loop through each sheet
  for (sheet in names(df)) {
    message(sheet)
    
    # Pull rules from the respective YAML, and store failing validation checks
    rules <- validate::validator(.file=paste0("input/rules/", sheet,".yaml"))
    out <- validate::confront(df[[sheet]], rules, ref=list(ref_ids=ref_ids))
    fails = validate::summary(out) %>% 
      dplyr::filter(fails > 0)
    
    # Loop through each failure and log the message
    for (i in seq_len(nrow(fails))) {
      m = validate::meta(rules[fails$name[i]])$message
      message(m)
      log_CvT_doc_load(f=f, m=m, log_path=log_path)
    }
  }
}