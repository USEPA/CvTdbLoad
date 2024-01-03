#' @#'@description Helper function to get the concentration medium dictionary form the CvT database
#' @description FUNCTION_DESCRIPTION

#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname dose_frequency_get_dict
#' @export 
dose_frequency_get_dict <- function(){
  return(db_query_cvt("SELECT * FROM cvt.dose_frequency_dict"))
}
