#' @title conc_medium_create_dict
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
#' @rdname conc_medium_create_dict
#' @export 
#' @importFrom dplyr mutate
conc_medium_create_dict <- function(overwrite = FALSE){
  if(overwrite){
    tmp = db_query_cvt("SELECT DISTINCT conc_medium_original, conc_medium_normalized FROM cvt.series") %>%
      dplyr::mutate(id := NA, .before=conc_medium_original,
             conc_medium_original = trimws(tolower(conc_medium_original)))
    
    push_tbl_to_db(dat=tmp,
                   tblName="conc_medium_dict",
                   overwrite=TRUE,
                   fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                  conc_medium_original="varchar(100)",
                                  conc_medium_normalized="varchar(100)"
                   ))  
  } else {
    message("...Set overwrite to 'TRUE' to overwrite existing concentration medium dictionary")
  }
}
