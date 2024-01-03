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
    message(sheet)
    # Pull rules from YAML files
    rules <- validate::validator(.file=paste0("input/rules/", sheet,".yaml"))
    
    # Special case check for fk_reference_document_id
    if (sheet == "Studies" && any(df$Documents$document_type == 2)) {
      ref_doc_ids = unique(df$Documents$id[df$Documents$document_type == 2])
      # All reference document entries must have corresponding ID used in any
      # of the Studies sheet fk_reference_document_id (must be at least 1, could be all)
      rule <- validate::validator(
        validate::exists_any(!ref_doc_ids %in% unique(fk_reference_document_id))
        )
      validate::meta(rule, "message") <- "unused_fk_reference_document_id"
      # https://www.rdocumentation.org/packages/validate/versions/1.1.3/topics/+,validator,validator-method
      rules <- rules + rule
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