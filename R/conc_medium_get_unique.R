#' @title conc_medium_get_unique
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
#'  [select][dplyr::select], [mutate][dplyr::mutate], [bind_rows][dplyr::bind_rows], [left_join][dplyr::left_join], [filter][dplyr::filter]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname conc_medium_get_unique
#' @export 
#' @importFrom dplyr select mutate bind_rows left_join filter
#' @importFrom writexl write_xlsx
conc_medium_get_unique <- function(fileList, template_path){
  #Get administration route from files
  cm = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Series %>% 
      dplyr::select(conc_medium_original = conc_medium) %>% 
      unique() %>%
      dplyr::mutate(filepath = f)
  }) %>% 
    dplyr::bind_rows()
  #Prep for matching
  out = cm %>%
    dplyr::mutate(conc_medium_original = trimws(tolower(conc_medium_original))) %>%
    unique() %>%
    #Attempt match
    dplyr::left_join(get_conc_medium_dict(), by="conc_medium_original") %>%
    dplyr::filter(is.na(conc_medium_normalized),
           !is.na(conc_medium_original)) %>%
    dplyr::select(filepath, conc_medium_original, conc_medium_normalized)
  
  #Output to file for curation
  if(nrow(out)){
    writexl::write_xlsx(out, paste0("input/conc_medium/conc_medium_to_curate_",Sys.Date(),".xlsx"))  
  } else {
    message("...No new concentration media to curate...returning...")
  }
  return(out)
}
