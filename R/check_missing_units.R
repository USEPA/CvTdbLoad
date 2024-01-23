#' @description Helper function to check for missing units for desired metric column.
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param units_col The units column to check #'
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
#' @rdname check_missing_units
#' @export 
#' @importFrom dplyr filter
check_missing_units <- function(x, f, units_col, log_path){
  x$missing_units = x$raw %>% dplyr::filter(!!as.symbol(units_col) %in% c("missing_units", "NA", "n/a", "N/A")  |
                                       is.na(!!as.symbol(units_col)))
  if(nrow(x$missing_units)){
    message("...Needs further curation: Missing - ", units_col)
    log_CvT_doc_load(f=f, m=paste0("curation_needed_",units_col,"_units"), log_path=log_path)
  }
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$missing_units$tempID)
  return(x)
}
