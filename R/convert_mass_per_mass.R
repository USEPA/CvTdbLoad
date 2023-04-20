#' @description Helpfer function to help convert mass/mass concentrations
#' @title FUNCTION_TITLE
#' @param x PARAM_DESCRIPTION
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
#'  [select][dplyr::select], [mutate][dplyr::mutate], [rename][dplyr::rename], [left_join][dplyr::left_join], [across][dplyr::across]
#' @rdname convert_mass_per_mass
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select mutate rename left_join across
convert_mass_per_mass <- function(x){
  #Filter density table to matching conc_medium and species
  density_table = readxl::read_xlsx("input/httk_tissue_density.xlsx") %>%
    dplyr::select(-Reference) %>%
    dplyr::mutate(Tissue = tolower(Tissue)) %>%
    dplyr::rename(value = `Density (g/cm^3)`)
  # density_table = httk::tissue.data %>%
  #   mutate(across(c("Species", "Tissue"), tolower)) %>%
  #   filter(grepl("Vol", variable),
  #          Species %in% unique(x$species),
  #          Tissue %in% unique(x$conc_medium) )%>% 
  #   select(-Reference, -variable)
  
  x = x %>%
    dplyr::left_join(density_table , by=c("conc_medium"="Tissue")) %>%
    dplyr::mutate(dplyr::across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), as.numeric))
  
  for(t in c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")){
    for(i in seq_len(nrow(x))){
      if(!is.na(x$value[i])){
        x[i,] = convert_units(x=x[i,], 
                              num=t, 
                              units="conc_units_original", desired="ug/ml",
                              overwrite_units = FALSE,
                              MW=(x$value[i])) #g/mL
        
      }
      
    }
  }
  return(x %>% dplyr::select(-value))
}
