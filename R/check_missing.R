#' @description Helper function to check for missing values for desired column.
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param miss_col The column to check
#' @param flag Whether to log a flag for the missing column #'
#' @param log_path File path where to save the log file.
#' @return Modified version of the input `x` parameter
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter]
#' @rdname check_missing
#' @export 
#' @importFrom dplyr filter
check_missing <- function(x, miss_col, f, flag=FALSE, log_path){
  x$missing = x$raw %>% dplyr::filter(!!as.symbol(miss_col) %in% c("NA", "n/a", "N/A")  |
                                       is.na(!!as.symbol(miss_col)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$missing$tempID)
  if(flag & nrow(x$missing)){
    message("...Needs further curation: Missing - ", miss_col)
    log_CvT_doc_load(f=f, m=paste0("missing_",miss_col,"_values"), log_path=log_path)
  }
  return(x)
}
