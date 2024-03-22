#' @title conc_medium_update_dict
#' @description FUNCTION_DESCRIPTION
#' @param dict_file PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx]
#'  [select][dplyr::select], [mutate][dplyr::mutate], [filter][dplyr::filter], [anti_join][dplyr::anti_join]
#' @rdname conc_medium_update_dict
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select mutate filter anti_join
dose_frequency_update_dict <- function(dict_file){
  stop("LOGIC STILL IN DEVELOPMENT. JUST COPIED FROM conc_medium_update_dict.R")
  if(file.exists(dict_file)){
    new = readxl::read_xlsx(dict_file) %>%
      dplyr::select(-id, -units) %>%
      dplyr::mutate(conc_medium_original = trimws(tolower(conc_medium_original))) %>%
      dplyr::filter(!is.na(conc_medium_normalized), conc_medium_normalized != "NA") %>%
      dplyr::anti_join(get_conc_medium_dict() %>% 
                         dplyr::select(conc_medium_original, conc_medium_normalized) %>%
                         dplyr::mutate(conc_medium_original = trimws(tolower(conc_medium_original)))) %>%
      dplyr::select(conc_medium_original, conc_medium_normalized)
    
    db_push_tbl_to_db(dat=new,
                      tblName="conc_medium_dict",
                      overwrite=FALSE, append=TRUE)
  } else {
    message("...input dict_file does not exist...cannot push dictionary")
  }
}
