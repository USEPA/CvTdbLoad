#' @title check_convert_failed
#' @description Check if a conversion failed, resulting in an NA value. This can be due to a lack of necessary equation parameters (e.g., molecular weight, subject weight).
#' @param x Input list of datasets being processed.
#' @param f Optional filename for logging purposes.
#' @param col String of name of column to check.
#' @param log_path File path where to save the log file.
#' @param id_col Column to use to log index value to help with logging.
#' @return Modified input `x` dataframe list with new "convert_failed" dataframe.
#' @seealso 
#'  [filter][dplyr::filter]
#' @rdname check_convert_failed
#' @export 
#' @importFrom dplyr filter
check_convert_failed <- function(x, f, col, log_path, id_col="id"){
  x$convert_failed = x$convert_ready %>% dplyr::filter(is.na(!!as.symbol(col)))
  if(nrow(x$convert_failed)){
    message("...",col," conversion failed...")
    log_CvT_doc_load(f=f, m=paste0("cvt_",col,"_convert_fail"), log_path=log_path, val=unique(x$convert_failed[[id_col]]))
  }
  x$convert_ready = x$convert_ready %>% dplyr::filter(!tempID %in% x$convert_failed$tempID)
  return(x)
}
