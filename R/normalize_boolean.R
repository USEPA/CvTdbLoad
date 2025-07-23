#' @title normalize_boolean
#' @description FUNCTION_DESCRIPTION
#' @param x Input dataframe
#' @param col Stirng vector of fields to update boolean values to "0" or "1".
#' @return Modified dataframe `x` with boolean fields updated to "0" or "1".
#' @rdname normalize_boolean
#' @export 
normalize_boolean <- function(x, col){
  # Loop through if multiple columns are provided
  for(a in col){
    # Convert column of missing values (imported as logical) to character columns
    if(all(is.na(x[[a]])) & typeof(x[[a]]) == "logical"){
      x[[a]] = as.character(x[[a]])
    }
    x[(!is.na(x[[a]]) & x[[a]] != "0" & x[[a]] != 0), a] <- "1"
    x[is.na(x[[a]]), a] <- "0"
    x[[a]] = as.numeric(x[[a]])
  }
  return(x)
}
