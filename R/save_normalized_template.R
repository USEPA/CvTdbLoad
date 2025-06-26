#' @description A helper function to cache the normalized templates during the normalization workflow.
#' @title save_normalized_template
#' @param df Named list of dataframes of normalized template data.
#' @param f Filename of the input template that is modified to save a normalized copy.
#' @return None. Input dataframe is saved to output folder.
#' @seealso 
#'  [file_ext][tools::file_ext]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname save_normalized_template
#' @export 
#' @importFrom tools file_ext
#' @importFrom writexl write_xlsx
save_normalized_template <- function(df, f){
  #"L:\Lab\HEM\T_Wall_Projects_FY20\CvT Database\output\normalized_templates"  
  fn = basename(f)
  fn_ext = tools::file_ext(fn)
  fn = gsub(paste0(".", fn_ext), "", fn)
  writexl::write_xlsx(x=doc_sheet_list, path=paste0("output/normalized_templates/", basename(fn), "_normalized.", fn_ext))
}
