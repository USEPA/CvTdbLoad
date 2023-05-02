#' @description Helper function to check if a metric entry has a ; separated list, therefore should be split.
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param col The column being checked/normalized #'
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
#' @rdname check_subject_list
#' @export 
#' @importFrom dplyr filter
check_subject_list <- function(x,f, col){
  #List of weights
  x$split_subject = x$raw %>% dplyr::filter(grepl(";|, ", !!as.symbol(col)))
  if(nrow(x$split_subject)){
    message("...Needs further curation: ", paste0(x$split_subject[[col]], collapse=";"))
    log_CvT_doc_load(f=f, m="curation_needed_split_subject")
  }
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$split_subject$tempID)
  return(x)
}
