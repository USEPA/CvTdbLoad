#' @description Function to check if processed document is missing required fields.
#' @param df List of dataframes for the sheets within an extraction template
#' @param f Filename for flagging purposes #'
#' @return None. Logs any flags
#' @title FUNCTION_TITLE
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
check_required_fields_new <- function(df, f){
  # Loop through each sheet and individual rules
  for (sheet in names(df)) {
    rules <- validate::validator(.file=paste0("input/rules/", sheet,".yaml"))
    out <- validate::confront(df[[sheet]], rules)
    fails = summary(out) %>% dplyr::filter(fails == 1)
    
    # Loop through each failure and log the message
    if (lengths(fails)[1] > 0) {
      for (i in 1:nrow(fails)) {
        m = meta(rules[fails[i]$name])$message
        log_CvT_doc_load(f=f, m=m)
      }
    }
  }
}