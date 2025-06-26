#' @title extract_units
#' @description Generic function to extract units from input columns. The goal is to try to fill in missing unit values from `units_col` based on string values in `conv_col`.
#' @param x Input dataframe.
#' @param units_col String of the name of the column containing units information with missing values.
#' @param conv_col String of the name of the column to try to extract unit information from.
#' @param unit_type Input unit type (e.g., weight, height, age, dose_duration, conc), used by `convert_units_grepl()`.
#' @return Modified version of input `x` dataframe where the input `units_col` will have filled in units values from input `conv_col` or be "missing_units" tagged instead of NA.
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [across][dplyr::across], [bind_rows][dplyr::bind_rows]
#'  [all_of][dplyr::all_of]
#' @rdname extract_units
#' @export 
#' @importFrom dplyr filter mutate across bind_rows
#' @importFrom tidyr all_of
#' @importFrom purrr flatten
#' @importFrom stringr str_squish
#' @importFrom mgsub mgsub
extract_units <- function(x, units_col, conv_col, unit_type){
  
  conv_list = convert_units_grepl(unit_type)
  rm_list = conv_list %>% 
    purrr::flatten() %>% 
    unlist() %>%
    paste0(".*", collapse="|")
  out_units = list()
  
  # NA in units and no units in original value field
  out_units$missing_units = x %>% 
    # Match whole-words for unit conversion list
    dplyr::filter(!grepl(paste0("\\b", conv_list %>% unlist() %>% unname(), "\\b", 
                                collapse="|"), 
                         !!as.symbol(units_col)))
  x = x %>% dplyr::filter(!tempID %in% out_units$missing_units$tempID)
  out_units$conv_ready = x %>% 
    dplyr::filter(grepl(paste0(conv_list %>% unlist() %>% unname(), collapse="|"), !!as.symbol(units_col)))
  x = x %>% dplyr::filter(!tempID %in% out_units$conv_ready$tempID)
  
  if(nrow(out_units$missing_units)){
    # Attempt to extract from conv_col
    out_units$missing_units[[units_col]] = lapply(seq_len(nrow(out_units$missing_units)), function(i){
      for(conv in names(conv_list)){
        if(grepl(conv_list[[conv]], out_units$missing_units[[conv_col]][i])){
          return(conv)
        }
      }
      return("missing_units")
    }) %>% unlist()
    # Remove units from converted column
    out_units$missing_units = out_units$missing_units %>%
      # https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
      dplyr::mutate(dplyr::across(.cols=dplyr::all_of(conv_col), 
                                  .fns= ~ stringr::str_squish(.) %>%
                                    # https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r/24173271
                                    gsub("\\s*\\([^\\)]+\\)","", .) %>%
                                    gsub(rm_list, "", .) %>%
                                    gsub("old|>|<|at least", "", .) %>%
                                    stringr::str_squish()
      ))
  }
  
  # Remove empty list elements
  out_units = out_units[sapply(out_units, nrow) > 0] %>%
    dplyr::bind_rows()
  
  # Normalize units
  norm_units = conv_list
  norm_units = lapply(names(norm_units), function(n_unit){
    tmp = norm_units[[n_unit]] %>%
      gsub("\\b", "", ., fixed = TRUE) %>%
      strsplit("|", fixed = TRUE) %>%
      unlist()
  }) %T>% {
    names(.) <- names(norm_units)
  }
  
  # Normalize units field based on conv_list
  for(n_unit in names(norm_units)){
    out_units = out_units %>%
      dplyr::mutate(!!units_col := mgsub::mgsub(!!as.name(units_col), 
                                                pattern = norm_units[[n_unit]], 
                                                replacement = rep(n_unit, length(norm_units[[n_unit]])))
                    )  
  }
  
  return(out_units)
}
