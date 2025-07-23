#' @title get_cvt_template
#' @description Load the CvT template file into a list of empty dataframes.
#' @param template_path Path to the CvT template file.
#' @return Named list of template file dataframes.
#' @seealso 
#'  [excel_sheets][readxl::excel_sheets], [read_xlsx][readxl::read_xlsx]
#' @rdname get_cvt_template
#' @export 
#' @importFrom readxl excel_sheets read_xlsx
get_cvt_template <- function(template_path){
  s_list = readxl::excel_sheets(template_path)
  lapply(s_list, function(s){
    readxl::read_xlsx(template_path, sheet=s)
  }) %T>% { names(.) <- s_list } %>%
    return()
}
