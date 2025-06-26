#' @title normalize_time
#' @description Normalize time to hours for input dataframe.
#' @param raw Input dataframe of data with data to normalize.
#' @param f Optional filename for logging purposes.
#' @param log_path File path where to save the log file.
#' @param debug Boolean of whether to stop conversion logic early for debugging purpose. Default: FALSE.
#' @return Modified dataframe of `raw` with normalized values.
#' @seealso 
#'  [mutate][dplyr::mutate], [bind_rows][dplyr::bind_rows], [arrange][dplyr::arrange]
#' @rdname normalize_time
#' @export 
#' @importFrom dplyr mutate bind_rows arrange
normalize_time <- function(raw, f, log_path, debug = FALSE){
  message("...normalizing conc time...")
  if(!nrow(raw)){# Empty dataframe
    message("...normalize_time dataframe empty...returning...")
    return(raw)
  }
  # List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c())
  out$raw = out$raw %>% dplyr::mutate(time_hr=as.numeric(NA))
  # Missing time values
  out = check_missing(x=out, miss_col = "time_original", f=f, flag=TRUE, log_path=log_path)
  
  # Missing units
  out = check_missing_units(x=out, f=f, units_col="time_units_original", log_path=log_path)
  # Normalize units
  out$raw$time_units_original = normalize_time_units(out$raw$time_units_original)
  # Non-numerics
  out = check_non_numeric(x=out, f=f, col="time_original", log_path=log_path)
  # Prep for conversion
  out$convert_ready = out$raw %>% dplyr::mutate(time_hr = as.numeric(time_original))
  
  out$raw = NULL
  # Convert time
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="time_hr", 
                                          units="time_units_original", desired="hr",
                                          overwrite_units = FALSE)
  }
  # Convert Failed
  out = check_convert_failed(x=out, f=f, col="time_hr", log_path=log_path)
  
  if (isTRUE(debug)) {
    return(out)
  }
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  # Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("convert_ready")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("time_hr")) %>%
        return()
    }
  })
  return(out %>% dplyr::bind_rows() %>% dplyr::arrange(tempID))
}
