#' @title normalize_weight
#' @description A helper function to normalize dose.
#' @param raw Input dataframe of data with data to normalize.
#' @param f Optional filename for logging purposes.
#' @param log_path File path where to save the log file.
#' @param debug Boolean of whether to stop conversion logic early for debugging purpose. Default: FALSE.
#' @return Normalized version of the input `raw` parameter.
#' @seealso 
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [bind_rows][dplyr::bind_rows], [arrange][dplyr::arrange], [select][dplyr::select]
#' @rdname normalize_weight
#' @export 
#' @importFrom dplyr mutate filter bind_rows arrange select
normalize_weight <- function(raw, f, log_path, debug=FALSE){
  message("...normalizing weight...")
 
  if(!nrow(raw)){# Empty dataframe
    message("...normalize_weight dataframe empty...returning...")
    return(raw)
  }
  # List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c("weight_kg"))
  # Set to convert column to maintain original
  out$raw$weight_kg = out$raw$weight
  # Extract units
  out$raw = extract_units(x=out$raw, units_col="weight_units", 
                          conv_col="weight_kg", unit_type="weight")

  # Missing weight
  out = check_missing(x=out, miss_col = "weight", f=f, log_path=log_path)
  # Missing units
  out = check_missing_units(x=out, f=f, units_col="weight_units", log_path=log_path)
  if(nrow(out$missing_units)){
    out$missing_units$weight_units = NA # Replacing missing units with NA after flagging  
  }
  
  # List of weights
  out = check_subject_list(x=out, f=f, col="weight_kg", log_path=log_path)
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="weight_kg", log_path=log_path)
  # Weight range
  out = check_unit_range(x=out, f=f, col="weight_kg", log_path=log_path)
  # Ready for conversion
  out$conversion = out$raw %>% 
    dplyr::mutate(weight_kg = suppressWarnings(as.numeric(weight_kg))) %>%
    dplyr::filter(!is.na(weight_kg))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$conversion$tempID)
  
  if(nrow(out$raw)){
    message("...Unhandled cases for weight: ", paste0(out$raw$weight_kg %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_weight_normalize_case", log_path=log_path, val=out$raw$id)
  }
  
  if (isTRUE(debug)){
    return(out)
  }
  
  # out$unhandled_cases = out$raw
  # Convert kg, g, mg, lbs, etc.
  out$convert_ready = dplyr::bind_rows(out$conversion, out$ci, out$unit_range)
  out$conversion = NULL; out$ci = NULL; out$unit_range = NULL
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="weight_kg", 
                                          units="weight_units", desired="kg",
                                          overwrite_units = FALSE)
  }
  
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  
  # Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("convert_ready", "unit_range")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("weight_kg")) %>%
        return()  
    }
  })
  
  # Convert weight_kg to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% dplyr::mutate(weight_kg = suppressWarnings(as.numeric(weight_kg)))
  })
  
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  
  out = out %>%
    dplyr::bind_rows() %>% dplyr::arrange(tempID) %>% dplyr::select(-tempID)
  out$weight_units[out$weight_units == "missing_units"] = NA # Replace missing_units with NA after flagging
  return(out)
}
