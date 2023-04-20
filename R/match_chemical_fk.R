#' @description Helper function TBD.
#' @title FUNCTION_TITLE
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname match_chemical_fk
#' @export 
match_chemical_fk <- function(df){
  
  chems = db_query_cvt("SELECT DISTINCT dsstox_substance_id FROM cvt.chemicals")
  
}
