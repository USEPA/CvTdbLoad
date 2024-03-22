#' @title administration_route_update_dict
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
#' @rdname administration_route_update_dict
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select mutate filter anti_join
administration_route_update_dict <- function(dict_file){
  if(file.exists(dict_file)){
    #Filter to only new entries to append to dictionary
    new = readxl::read_xlsx(dict_file) %>%
      dplyr::select(-id) %>%
      dplyr::mutate(administration_route_original = trimws(tolower(administration_route_original))) %>%
      dplyr::filter(!is.na(administration_route_normalized), administration_route_normalized != "NA") %>%
      dplyr::anti_join(get_administration_route_dict() %>% 
                  dplyr::select(administration_route_original, administration_route_normalized) %>%
                  dplyr::mutate(administration_route_original = trimws(tolower(administration_route_original))))
    #Push new dictionary entries
    db_push_tbl_to_db(dat=new,
                  tblName="administration_route_dict",
                  overwrite=FALSE, append=TRUE)
  } else {
    message("...input dict_file does not exist...cannot push dictionary")
  }
}
