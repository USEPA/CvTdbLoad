#' @title species_get_unique_to_curate
#' @description Load a list of templates or query the database to check for species cases to add to `normalize_species()`.
#' @param fileList Optional list of template files to load. If NULL, queries the database to check for cases.
#' @param template_path Path to input CvTdb template file.
#' @return Console output of species cases to add to `normalize_species()`.
#' @seealso 
#'  [select][dplyr::select]
#' @rdname species_get_unique_to_curate
#' @export 
#' @importFrom dplyr select
species_get_unique_to_curate <- function(fileList, template_path){
  
  if(!is.null(fileList)){
    # Get species from files
    spec = lapply(fileList, function(f){
      s_list = load_sheet_group(fileName = f, template_path = template_path)
      s_list$Subjects %>% dplyr::pull(species) %>% unique()
    }) %>% 
      unlist() %>%
      unique()
    
    out = normalize_species(spec) %>%
      unique()  
  } else {
    # If no input files, query the database to check for cases to curate
    out = db_query_cvt("SELECT distinct species FROM cvt.subjects") %>%
      normalize_species() %>%
      dplyr::pull(species) %>%
      unique()
  }
  
  return(out[!out %in% c("dog", "human", "mouse", "monkey", 
                         "rat", "rabbit", "guinea pig", "frog", "hamster")])
}
