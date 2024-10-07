#' @title species_get_unique_to_curate
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
#'  [select][dplyr::select]
#' @rdname species_get_unique_to_curate
#' @export 
#' @importFrom dplyr select
species_get_unique_to_curate <- function(fileList, template_path){
  
  if(!is.null(fileList)){
    #Get species from files
    spec = lapply(fileList, function(f){
      s_list = load_sheet_group(fileName = f, template_path = template_path)
      s_list$Subjects %>% dplyr::pull(species) %>% unique()
    }) %>% 
      unlist() %>%
      unique()
    
    out = normalize_species(spec) %>%
      unique()  
  } else {
    out = db_query_cvt("SELECT distinct species FROM cvt.subjects") %>%
      normalize_species() %>%
      dplyr::pull(species) %>%
      unique()
  }
  
  return(out[!out %in% c("dog", "human", "mouse", "monkey", 
                         "rat", "rabbit", "guinea pig", "frog", "hamster")])
}
