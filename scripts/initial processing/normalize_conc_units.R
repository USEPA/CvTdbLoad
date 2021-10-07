#'@description A helper function to normalize concentration units.
#'@param raw A dataframe of weight information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_conc_units <- function(raw, f){
  message("...normalizing conc_units...")
  #weight_estimated --> 0 = No estimation (just conversion); 1 = extrapolated; 2 = range mean
  tmp = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Series %>% 
      left_join(s_list$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
      select(id, conc_medium, conc_original=conc, conc_units_original=conc_units) %>%
      mutate(doc = f)
  }) %>%
    bind_rows()
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_conc_units dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = prep_normalization(x=raw, newcols=c())
  out$raw = out$raw %>% mutate(conc=as.numeric(NA))
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="conc_units_original")
  #Normalize units
  out$raw$time_units_original = normalize_conc_units(out$raw$conc_units_original)
  #Non-numerics
  out = check_non_numeric(x=out, f=f, col="conc_original")
  #Prep for conversion
  out$convert_ready = out$raw %>% mutate(conc = as.numeric(conc_original))
  out$raw = NULL
  #remaining logic TBD
  message("...Conc_units conversion logic TBD")
  warning("...Conc_units conversion logic TBD")
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="conc", 
                                          units="conc_units", desired="",
                                          overwrite_units = FALSE)
  }
  
  #Convert weight_kg to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% mutate(conc = suppressWarnings(as.numeric(conc)))
  })
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  return(out %>% bind_rows() %>% arrange(tempID) %>% select(-tempID))
}

normalize_conc_units <- function(x){
  #Convert conc units
  x = tolower(x)
  conv = list()
  
  x = lapply(x, function(s){
    for(c in names(conv)){
      if(!s %in% conv[[c]]){
        next
      } else {
        return(c)  
      }
    }
    return(s)
  }) %>% unlist()
  return(x)
}
