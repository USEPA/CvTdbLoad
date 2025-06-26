#' @title chemical_curation_prep_name_match
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
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
#'  [str_extract_all][stringr::str_extract_all]
#' @rdname chemical_curation_prep_name_match
#' @export 
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract_all
chemical_curation_prep_name_match <- function(df){
  #Extract parentheses
  df = df %>%
    dplyr::mutate(name_parentheses = gsub("\\(([^()]+)\\)", "\\1", stringr::str_extract_all(name, "\\(([^()]+)\\)")),
           name_no_parentheses = gsub("\\(([^()]+)\\)", "", name),
           name_secondary_parentheses = gsub("\\(([^()]+)\\)", "\\1", stringr::str_extract_all(name_secondary, "\\(([^()]+)\\)")),
           name_secondary_no_parentheses = gsub("\\(([^()]+)\\)", "", name_secondary))
  df$name_parentheses[df$name_parentheses == "character0"] = NA
  df$name_parentheses[df$name_secondary_parentheses == "character0"] = NA
  return(df)
}
