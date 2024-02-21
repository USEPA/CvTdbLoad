#' @description Helper function to check for missing metric and attemp to exrapolate values.
#' Eventually make this a generic function for all norm extrapolation processes...
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param extrap_type The type of extrapolation being performed (weight is the only accepted form at this time). #'
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
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [across][dplyr::across], [group_by][dplyr::group_by], [summarise][dplyr::summarise], [select][dplyr::select], [left_join][dplyr::left_join], [distinct][dplyr::distinct], [rename][dplyr::rename]
#' @rdname norm_extrapolate
#' @export 
#' @importFrom dplyr filter mutate across group_by summarise select left_join distinct rename
norm_extrapolate <- function(x, f, extrap_type, log_path){
  #weight Group that needs extrapolation based on similar species/subtype
  x$extrapolate = x$raw %>% dplyr::filter(is.na(!!as.symbol(extrap_type)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$extrapolate$tempID)
  
  # Removed weight extrapolation logic
  
  return(x)
}
