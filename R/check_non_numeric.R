#' @title check_non_numeric
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [select][dplyr::select]
#' @rdname check_non_numeric
#' @export 
#' @importFrom dplyr mutate filter select
check_non_numeric <- function(x, f, col){
  x$non_numeric = x$raw %>%
    dplyr::mutate(non_numeric_check = suppressWarnings(as.numeric(gsub(",", "", !!as.symbol(col))))) %>%
    dplyr::filter(is.na(non_numeric_check)) %>%
    dplyr::select(-non_numeric_check)
  if(nrow(x$non_numeric)){
    message("...Non-numeric ", col," value found...need to handle...")
    log_CvT_doc_load(f=f, m=paste0("unhandled_cvt_",col,"_non_numeric"))
  }
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$non_numeric$tempID)
  return(x)
}
