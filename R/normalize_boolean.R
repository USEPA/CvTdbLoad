#' @title normalize_boolean
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname normalize_boolean
#' @export 
normalize_boolean <- function(x, col){
  #If multiple columns are provided
  
  for(a in col){
    #Convert column of missing values (imported as logical) to character columns
    if(all(is.na(x[[a]])) & typeof(x[[a]]) == "logical"){
      x[[a]] = as.character(x[[a]])
    }
    x[(!is.na(x[[a]]) & x[[a]] != "0" & x[[a]] != 0), a] <- "1"
    x[is.na(x[[a]]), a] <- "0"
    x[[a]] = as.numeric(x[[a]])
  }
  return(x)
}
