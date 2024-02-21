#' @description Helper function to check for ranges, and handling them
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param col The column being checked/normalized
#' @param estimated The column for *_estimated flags (0, 1, 2) #'
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
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [rowwise][dplyr::rowwise], [across][dplyr::across], [select][dplyr::select]
#'  [separate][tidyr::separate], [all_of][tidyr::all_of]
#' @rdname check_unit_range
#' @export 
#' @importFrom dplyr filter mutate rowwise across select
#' @importFrom tidyr separate all_of
check_unit_range <- function(x, f, col, estimated, log_path){
  x$unit_range = x$raw %>% dplyr::filter(grepl("[\\-]|to|\\-|-|-|and|or", !!as.symbol(col)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$unit_range$tempID)
  
  #Normalize weight groups
  if(nrow(x$unit_range)){
    x$unit_range = x$unit_range %>% 
      #mutate(tmp = gsub("kg|kilograms|kilo|kilogram", "", weight)) %>%
      tidyr::separate(!!as.symbol(col), c("lower", "upper"), sep="[\\-]|to|\\-|-|-|and|or", remove=FALSE)
    #Check for numeric conversion issues
    tryCatch({as.numeric(gsub(",", "", x$unit_range$lower))},
             warning=function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"), log_path=log_path, val=x$unit_range$id)
             })
    tryCatch({as.numeric(gsub(",", "", x$unit_range$upper))},
             warning=function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"), log_path=log_path, val=x$unit_range$id)
             })
    x$unit_range = x$unit_range %>%
      dplyr::mutate(lower = as.numeric(gsub(",", "", lower)), #Remove "," place separator
             upper = as.numeric(gsub(",", "", upper))) %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~mean(c(upper, lower), na.rm=T))) %>%
      dplyr::select(-upper, -lower)
    if(length(estimated)){
      x$unit_range[[estimated]] = 2
    }
  } else {
    x$unit_range = x$unit_range %>% 
      dplyr::mutate(dplyr::across(.cols=tidyr::all_of(col), .fns = ~suppressWarnings(as.numeric(!!as.symbol(col)))))
  }
  
  return(x)
}
