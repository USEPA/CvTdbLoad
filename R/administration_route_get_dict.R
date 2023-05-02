#' @description Helper function to get the administration route dictionary form the CvT database
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname administration_route_get_dict
#' @export 
administration_route_get_dict <- function(){
  return(db_query_cvt("SELECT * FROM cvt.administration_route_dict"))
}
