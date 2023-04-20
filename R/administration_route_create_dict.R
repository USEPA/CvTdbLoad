#' @title administration_route_create_dict
#' @description FUNCTION_DESCRIPTION
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [mutate][dplyr::mutate]
#' @rdname administration_route_create_dict
#' @export 
#' @importFrom dplyr mutate
administration_route_create_dict <- function(overwrite = FALSE){
  if(overwrite){
    tmp = db_query_cvt("SELECT DISTINCT administration_route_original, administration_route_normalized FROM cvt.studies") %>%
      dplyr::mutate(id := NA, .before=administration_route_original,
             administration_route_original = trimws(tolower(administration_route_original)))
    
    push_tbl_to_db(dat=tmp,
                   tblName="administration_route_dict",
                   overwrite=TRUE,
                   fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                  administration_route_original="varchar(100)",
                                  administration_route_normalized="varchar(100)"
                   ))  
  } else {
    message("...Set overwrite to 'TRUE' to overwrite existing administration route dictionary")
  }
}
