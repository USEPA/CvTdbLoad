#' @title check_non_numeric
#' @description Function to check for non-numeric values for input column.
#' @param x Input list of datasets being processed.
#' @param f Optional filename for logging purposes.
#' @param col String of the name of the column to check.
#' @param log_path File path where to save the log file.
#' @return Modified input `x` dataframe list with new "non_numeric" dataframe.
#' @seealso 
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [select][dplyr::select]
#' @rdname check_non_numeric
#' @export 
#' @importFrom dplyr mutate filter select
check_non_numeric <- function(x, f, col, log_path){
  x$non_numeric = x$raw %>%
    dplyr::mutate(non_numeric_check = suppressWarnings(as.numeric(gsub(",", "", !!as.symbol(col))))) %>%
    dplyr::filter(is.na(non_numeric_check)) %>%
    dplyr::select(-non_numeric_check)
  if(nrow(x$non_numeric)){
    message("...Non-numeric ", col," value found...need to handle...")
    log_CvT_doc_load(f=f, m=paste0("unhandled_cvt_",col,"_non_numeric"), log_path=log_path, val=x$non_numeric$id)
  }
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$non_numeric$tempID)
  return(x)
}
