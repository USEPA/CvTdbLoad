#' @title check_unit_ci
#' @description Function to check for confidence intervals for an input field.
#' @param x Input list of datasets being processed.
#' @param f Optional filename for logging purposes.
#' @param col String of the name of the column to check.
#' @param log_path File path where to save the log file.
#' @return Modified input `x` dataframe list with new "ci" dataframe.
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [across][dplyr::across]
#'  [all_of][tidyr::all_of]
#' @rdname check_unit_ci
#' @export 
#' @importFrom dplyr filter mutate across
#' @importFrom tidyr all_of
#' @param log_path PARAM_DESCRIPTION
check_unit_ci <- function(x, f, col, log_path){
  #Removed |? at end of regex, not sure what it was used for??
  x$ci = x$raw %>% dplyr::filter(grepl("±|\\+/-|\\+", !!as.symbol(col)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$ci$tempID)
  
  if(nrow(x$ci)){
    x$ci = x$ci %>%
      dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~sub('±.*|\\+/-.*|\\+.*', '', !!as.symbol(col))))
    tryCatch({ x$ci %>% dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~as.numeric(gsub(",", "", !!as.symbol(col))))) },
             warning = function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"), log_path=log_path, val = x$ci$id)
             })
    x$ci = x$ci %>% dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~as.numeric(gsub(",", "", !!as.symbol(col)))))
  } else {
    x$ci = x$ci %>% dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~suppressWarnings(as.numeric(!!as.symbol(col)))))
  }
  return(x)
}
