#' @description Generic function to extract units from input columns
#' @title FUNCTION_TITLE
#' @param x PARAM_DESCRIPTION
#' @param units_col PARAM_DESCRIPTION
#' @param conv_col PARAM_DESCRIPTION
#' @param unit_type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [across][dplyr::across], [bind_rows][dplyr::bind_rows]
#'  [all_of][tidyr::all_of]
#' @rdname extract_units
#' @export 
#' @importFrom dplyr filter mutate across bind_rows
#' @importFrom tidyr all_of
extract_units <- function(x, units_col, conv_col, unit_type){
  #units_col = "weight_units"
  #conv_col = "weight_kg"
  #unit_type = "weight"
  conv_list = convert_units_grepl(unit_type)
  out_units = list()
  
  # #Has units field
  # out_units$has_units = x %>%
  #   filter(!is.na(!!as.symbol(units_col)))
  # x = x %>% filter(!tempID %in% out_units$tempID)
  
  #NA in weight_units and no units in weight_kg field
  out_units$missing_units = x %>% 
    dplyr::filter(!grepl(paste0(conv_list %>% unlist() %>% unname(), collapse="|"), !!as.symbol(units_col)))
  x = x %>% dplyr::filter(!tempID %in% out_units$missing_units$tempID)
  out_units$conv_ready = x %>% 
    dplyr::filter(grepl(paste0(conv_list %>% unlist() %>% unname(), collapse="|"), !!as.symbol(units_col)))
  x = x %>% dplyr::filter(!tempID %in% out_units$conv_ready$tempID)
  
  if(nrow(out_units$missing_units)){
    #Attempt to extract from conv_col
    #out_units$missing_units[[units_col]] = "missing_units"
    out_units$missing_units[[units_col]] = lapply(seq_len(nrow(out_units$missing_units)), function(i){
      for(conv in names(conv_list)[!names(conv_list) %in% c("rm_list")]){
        if(grepl(conv_list[[conv]], out_units$missing_units[[conv_col]][i])){
          return(conv)
        }
      }
      return("missing_units")
    }) %>% unlist()
    #Remove units from converted column
    out_units$missing_units = out_units$missing_units %>%
      #https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
      dplyr::mutate(dplyr::across(.cols=tidyr::all_of(conv_col), .fns= ~gsub(paste0(conv_list$rm_list, ".*", collapse="|"), "", .) %>%
                      gsub("old", "", .) %>%
                      #https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r/24173271
                      gsub("\\s*\\([^\\)]+\\)","", .) %>%
                      gsub(">|<|at least", "", .)
                    ))
  }
  
  #Remove empty list elements
  out_units = out_units[sapply(out_units, nrow) > 0]
  return(out_units %>% dplyr::bind_rows())
}
