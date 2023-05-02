#' @title check_radiolabel
#' @description FUNCTION_DESCRIPTION
#' @param raw PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate]
#' @rdname check_radiolabel
#' @export 
#' @importFrom dplyr filter mutate
check_radiolabel <- function(raw, f){
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     select(analyte_name, analyte_name_secondary, fk_study_id, radiolabeled) %>%
  #     left_join(s_list$Studies %>% select(id, test_substance_name), by=c("fk_study_id"="id")) %>%
  #     return()
  # }) %>%
  #   bind_rows()
  check = raw %>%
    dplyr::filter((is.na(radiolabeled) | radiolabeled != 1),
           !grepl("DTXSID", analyte_name),
           !grepl("DTXSID", analyte_name_secondary),
           !grepl("DTXSID", test_substance_name)) %>%
    dplyr::mutate(analyte_name_check = grepl("([1-9][0-9])", analyte_name),
           analyte_name_secondary_check = grepl("([1-9][0-9])", analyte_name_secondary),
           test_substance_name_check = grepl("([1-9][0-9])", test_substance_name)) %>%
    #If any of the chemical name fields contained 2 digit values
    dplyr::filter(analyte_name_check == TRUE | 
             analyte_name_secondary_check == TRUE | 
             test_substance_name_check == TRUE)
  if(nrow(check)){
    message("...chemicals or analyte found that need radiolabel set to 1")
    log_CvT_doc_load(f=f, m=paste0("potential_missing_radiolabel_detected"))
  }
}
