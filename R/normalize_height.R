#' @title normalize_height
#' @description A function to normalize subject height.
#' @param raw Input dataframe of data with data to normalize.
#' @param f Optional filename for logging purposes.
#' @param log_path File path where to save the log file.
#' @param debug Boolean of whether to stop conversion logic early for debugging purpose. Default: FALSE.
#' @return Normalized version of the input `raw` parameter.
#' @seealso 
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [bind_rows][dplyr::bind_rows], [arrange][dplyr::arrange]
#' @rdname normalize_height
#' @export 
#' @importFrom dplyr mutate filter bind_rows arrange
normalize_height <- function(raw, f, log_path, debug=FALSE){
  message("...normalizing height...")
  if(!nrow(raw)){#  Empty dataframe
    message("...normalize_height dataframe empty...returning...")
    return(raw)
  }
  #  List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c("height_cm"))
  #  Set to convert column to maintain original
  out$raw$height_cm = out$raw$height
  #  Extract units
  #  out$raw = extract_height_units(x=out$raw)
  out$raw = extract_units(x=out$raw, units_col="height_units", 
                          conv_col="height_cm", unit_type="height")

  # Missing height
  out = check_missing(x=out, miss_col = "height", f=f, flag=FALSE, log_path=log_path)
  #  Missing units
  out = check_missing_units(x=out, f=f, units_col="height_units", log_path=log_path, flag=FALSE)
  if(nrow(out$missing_units)){
    out$missing_units$height_units = NA #  Replacing missing units to NA after flagging  
  }
  
  #  List of heights
  out = check_subject_list(x=out, f=f, col="height_cm", log_path=log_path)
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="height_cm", log_path=log_path)
  #  Height range
  out = check_unit_range(x=out, f=f, col="height_cm", log_path=log_path)
  #  Ready for conversion
  out$conversion = out$raw %>% 
    dplyr::mutate(height_cm = suppressWarnings(as.numeric(height_cm))) %>%
    dplyr::filter(!is.na(height_cm))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$conversion$tempID)
  
  if(nrow(out$raw)){
    message("...Unhandled cases for height: ", paste0(out$raw$height_cm %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_height_normalize_case", log_path=log_path, val=out$raw$id)
  }
  
  if (isTRUE(debug)) {
    return(out)
  }
  #  out$unhandled_cases = out$raw
  #  Convert m, cm, mm, in, ft, etc.
  out$convert_ready = dplyr::bind_rows(out$conversion, out$ci, out$unit_range)
  out$conversion = NULL; out$ci = NULL; out$unit_range = NULL
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="height_cm", 
                                          units="height_units", desired="cm", 
                                          overwrite_units = FALSE)
  }
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  #  Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("convert_ready")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("height_cm")) %>%
        return()
    }
  })
  #  Convert height_cm to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% dplyr::mutate(height_cm = suppressWarnings(as.numeric(height_cm)))
  })
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  return(out %>% dplyr::bind_rows() %>% dplyr::arrange(tempID))
}
