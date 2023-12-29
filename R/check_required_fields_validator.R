#' @title Check required fields validator
#' @description Function to check if processed document is missing required fields.
#' @param df List of dataframes for the sheets within an extraction template
#' @param f Filename for flagging purposes
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
check_required_fields_validator <- function(df, f){
  # Loop through each sheet
  for (sheet in names(df)) {
    # Pull rules from YAML files
    rules <- validate::validator(.file=paste0("input/rules/", sheet,".yaml"))
    
    # Special case check for fk_reference_document_id
    if (sheet == "Series" && nrow(df[["Documents"]]) > 1) {
      rule <- validate::validator(!is.na(fk_reference_document_id))
      validate::meta(rule, "message") <- "NA_in_required_field_fk_reference_document_id"
      rules <- c(rules, rule)
    }
    
    # Obtain a dataframe of failing validation checks
    out <- validate::confront(df[[sheet]], rules)
    fails = validate::summary(out) %>% 
      dplyr::filter(fails > 0)
    
    # Loop through each failure and log the message
    for (i in seq_len(nrow(fails))) {
      m = validate::meta(rules[fails$name[i]])$message
      message(m)
      log_CvT_doc_load(f=f, m=m)
    }
  }
}