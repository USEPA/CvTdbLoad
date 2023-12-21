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
  # Loop through each sheet and individual rules
  for (sheet in names(df)) {
    rules <- validate::validator(.file=paste0("input/rules/", sheet,".yaml"))
    out <- validate::confront(df[[sheet]], rules)
    fails = validate::summary(out) %>% 
      dplyr::filter(fails > 0)
    
    # Loop through each failure and log the message
    if (nrow(fails)) {
      for (i in seq_len(nrow(fails))) {
        m = validate::meta(rules[fails$name[i]])$message
        log_CvT_doc_load(f=f, m=m)
      }
    }
  }
}