#' @description A helper function to cache the normalized templates
#' @title FUNCTION_TITLE
#' @param df PARAM_DESCRIPTION, Default: doc_sheet_list
#' @param f PARAM_DESCRIPTION, Default: f
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [file_ext][tools::file_ext]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname save_normalized_template
#' @export 
#' @importFrom tools file_ext
#' @importFrom writexl write_xlsx
save_normalized_template <- function(df=doc_sheet_list, f=f){
  #"L:\Lab\HEM\T_Wall_Projects_FY20\CvT Database\output\normalized_templates"  
  fn = basename(f)
  fn_ext = tools::file_ext(fn)
  fn = gsub(paste0(".", fn_ext), "", fn)
  writexl::write_xlsx(x=doc_sheet_list, path=paste0("output/normalized_templates/", basename(fn), "_normalized.", fn_ext))
}
