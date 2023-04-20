#' @title chemical_curation_get_curation_files
#' @description FUNCTION_DESCRIPTION
#' @param f_name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [excel_sheets][readxl::excel_sheets], [read_xlsx][readxl::read_xlsx]
#'  [filter][dplyr::filter]
#' @rdname chemical_curation_get_curation_files
#' @export 
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom dplyr filter
chemical_curation_get_curation_files <- function(f_name){
  f_sheets = readxl::excel_sheets(f_name)
  
  lapply(f_sheets, function(f){
    tmp2 = readxl::read_xlsx(f_name, sheet=f)
    if("FOUND_BY" %in% names(tmp2)){
      tmp2 = tmp2 %>%
        dplyr::filter(!grepl("warning|no_match", FOUND_BY, ignore.case = TRUE))
    }
    if(!"PREFERRED_NAME" %in% names(tmp2)){
      tmp2$PREFERRED_NAME = NA
    }
    if(!"CASRN" %in% names(tmp2)){
      tmp2$CASRN = NA
    }
    return(tmp2)
  }) %T>% { names(.) <- f_sheets } %>%
    return()
}
