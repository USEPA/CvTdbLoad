#' @title check_missing
#' @description Function to check for missing values for input column.
#' @param x Input list of datasets being processed.
#' @param f Optional filename for logging purposes.
#' @param miss_col String of the name of the column to check.
#' @param flag Whether to log a flag for the missing column.
#' @param log_path File path where to save the log file.
#' @return Modified input `x` dataframe list with new "missing" dataframe.
#' @seealso 
#'  [filter][dplyr::filter]
#' @rdname check_missing
#' @export 
#' @importFrom dplyr filter
check_missing <- function(x, miss_col, f, flag=TRUE, log_path){
  x$missing = x$raw %>% dplyr::filter(!!as.symbol(miss_col) %in% c("NA", "n/a", "N/A", "NR", "ND", "NQ", "NE", "NS")  |
                                       is.na(!!as.symbol(miss_col)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$missing$tempID)
  if(flag & nrow(x$missing)){
    message("...Needs further curation: Missing - ", miss_col)
    log_CvT_doc_load(f=f, m=paste0("missing_",miss_col,"_values"), log_path=log_path, val=x$missing$id)
  }
  return(x)
}
