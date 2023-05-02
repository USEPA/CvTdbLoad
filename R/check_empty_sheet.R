#' @description Function to check if any sheet of loaded template is empty
#' @param template List of loaded template sheets #'
#' @return Boolean TRUE or FALSE if an empty sheet exists
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [is_empty][purrr::is_empty]
#' @rdname check_empty_sheet
#' @export 
#' @importFrom purrr is_empty
check_empty_sheet <- function(template){
  if(purrr::is_empty(template)){
    return(TRUE)
  }
  for(s in template){
    if(!nrow(s)) return(TRUE)
  }
  return(FALSE)
}
