#' @title map_age_category
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param dict PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [select][dplyr::select]
#' @rdname map_age_category
#' @export 
#' @importFrom dplyr filter select
map_age_category <- function(x, dict){
  for(i in seq_len(nrow(x))){
    m_dict = dict %>% dplyr::filter(species == x$species[i]) %>% dplyr::select(-species)
    if(!grepl(m_dict$unit, x$age_units[i])){
      #Convert units to desired
      x[i,] = convert_units(x[i,], num="age_normalized", units="age_units", 
                            desired=m_dict$unit, overwrite_units = TRUE)
      if(is.na(x$age_normalized[i])){
        x$age_category[i] = "unhandled_age_unit_conversion"
        next
      }
    } 
    if(x$age_normalized[i] > (2 * m_dict$aged)){ #impossible ages > 2 * max category
      x$age_category[i] = "impossible_subject_age"
      next
    } else if(x$age_normalized[i] < m_dict$infant){ #neonate if less than infant category
      x$age_category[i] = "neonate"
      next
    }
    #Find the first case where the age is > the category checked (in reverse order)
    x$age_category[i] = rev(names(m_dict %>% dplyr::select(-unit)))[(which(x$age_normalized[i] >= rev(m_dict %>% dplyr::select(-unit)))[1])]
  }
  return(x)
}
