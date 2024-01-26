#' @title check_convert_failed
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @param log_path File path where to save the log file.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter]
#' @rdname check_convert_failed
#' @export 
#' @importFrom dplyr filter
check_convert_failed <- function(x, f, col, log_path){
  x$convert_failed = x$convert_ready %>% dplyr::filter(is.na(!!as.symbol(col)))
  if(nrow(x$convert_failed)){
    message("...",col," conversion failed...")
    log_CvT_doc_load(f=f, m=paste0("cvt_",col,"_convert_fail"), log_path=log_path, val=x$convert_failed$id)
  }
  x$convert_ready = x$convert_ready %>% dplyr::filter(!tempID %in% x$convert_failed$tempID)
  return(x)
}
