#' @title check_missing_units
#' @description Function to check for missing units for input column.
#' @param x Input list of datasets being processed.
#' @param f Optional filename for logging purposes.
#' @param units_col String of the name of the column to check.
#' @param log_path File path where to save the log file.
#' @param flag Whether to log a flag for the missing column.
#' @return Modified input `x` dataframe list with new "missing_units" dataframe.
#' @seealso 
#'  [filter][dplyr::filter]
#' @rdname check_missing_units
#' @export 
#' @importFrom dplyr filter
check_missing_units <- function(x, f, units_col, log_path, flag=TRUE){
  x$missing_units = x$raw %>% dplyr::filter(!!as.symbol(units_col) %in% c("missing_units", "NA", "n/a", "N/A")  |
                                       is.na(!!as.symbol(units_col)))
  if(flag & nrow(x$missing_units)){
    message("...Needs further curation: Missing - ", units_col)
    log_CvT_doc_load(f=f, m=paste0("curation_needed_",units_col,"_units"), log_path=log_path, val = x$missing_units$id)
  }
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$missing_units$tempID)
  return(x)
}
