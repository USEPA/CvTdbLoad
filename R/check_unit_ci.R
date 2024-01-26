#' @description Helper function to check for confidence intervals for a metric, and handling them.
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param col The column being checked/normalized
#' @param estimated The column for *_estimated flags (0, 1, 2) #'
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
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [across][dplyr::across]
#'  [all_of][tidyr::all_of]
#' @rdname check_unit_ci
#' @export 
#' @importFrom dplyr filter mutate across
#' @importFrom tidyr all_of
check_unit_ci <- function(x, f, col, estimated, log_path){
  #Removed |? at end of regex, not sure what it was used for??
  x$ci = x$raw %>% dplyr::filter(grepl("±|\\+/-|\\+", !!as.symbol(col)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$ci$tempID)
  
  if(nrow(x$ci)){
    x$ci = x$ci %>%
      dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~sub('±.*|\\+/-.*|\\+.*', '', !!as.symbol(col))))
    tryCatch({ x$ci %>% dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~as.numeric(gsub(",", "", !!as.symbol(col))))) },
             warning = function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"), log_path=log_path)
             })
    x$ci = x$ci %>% dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~as.numeric(gsub(",", "", !!as.symbol(col)))))
    if(length(estimated)){
      x$ci[[estimated]] <- 0
    }
  } else {
    x$ci = x$ci %>% dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~suppressWarnings(as.numeric(!!as.symbol(col)))))
  }
  return(x)
}
