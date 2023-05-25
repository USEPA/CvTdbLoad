#' @description Helper function to get the concentration medium dictionary form the CvT database
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname conc_medium_get_dict
#' @export 
conc_medium_get_dict <- function(){
  return(db_query_cvt("SELECT * FROM cvt.conc_medium_dict"))
}
