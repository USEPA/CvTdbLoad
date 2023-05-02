#' @description Helper function to add temp ID column and new empty columns before normalization.
#' @param x Input dataframe being prepped for normalization
#' @param newcols 
#'
#' @return Modified version of the input `x` parameter
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname normalization_prep
#' @export 
normalization_prep <- function(x, newcols){
  #Add unique identifier to help filter out select weight types
  x$tempID = seq_len(nrow(x))
  #Add empty column
  x[, newcols] = NA
  return(x)
}
