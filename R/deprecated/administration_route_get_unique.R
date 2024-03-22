#' @title administration_route_get_unique
#' @description FUNCTION_DESCRIPTION
#' @param fileList PARAM_DESCRIPTION
#' @param template_path PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [select][dplyr::select], [mutate][dplyr::mutate], [bind_rows][dplyr::bind_rows], [distinct][dplyr::distinct], [left_join][dplyr::left_join], [filter][dplyr::filter]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname administration_route_get_unique
#' @export 
#' @importFrom dplyr select mutate bind_rows distinct left_join filter
#' @importFrom writexl write_xlsx
administration_route_get_unique <- function(fileList, template_path){
  #Get administration route from files
  ar = lapply(fileList, function(f){
    message("Loading file: ", f)
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Studies %>% 
      dplyr::select(administration_route_original = administration_route) %>% 
      unique() %>%
      dplyr::mutate(filepath = f)
  }) %>% 
    dplyr::bind_rows()
  #Prep for matching
  out = ar %>%
    dplyr::mutate(administration_route_original = trimws(tolower(administration_route_original))) %>%
    dplyr::distinct() %>%
    #Attempt match
    dplyr::left_join(administration_route_get_dict(), by="administration_route_original") %>%
    dplyr::filter(is.na(administration_route_normalized)) %>%
    dplyr::select(filepath, administration_route_original, administration_route_normalized)
  
  #Output to file for curation
  if(nrow(out)){
    writexl::write_xlsx(out, paste0("input/administration_route/administration_route_to_curate_",Sys.Date(),".xlsx"))
  } else {
    message("...No new administration routes to curate...returning")  
  }
  return(out)
}
