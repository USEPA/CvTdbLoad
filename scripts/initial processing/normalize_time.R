
normalize_time <- function(raw, f){
  message("...normalizing conc time...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     left_join(s_list$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
  #     select(id, time_original = time, time_units_original = time_units) %>%
  #     mutate(doc = f)
  # }) %>%
  #   bind_rows()
  #Pull directly to handle more cases for conversion
  #tmp = query_cvt("SELECT c.id, s.id, c.fk_series_id, c.time_original, s.time_units_original, c.time_hr FROM series s LEFT JOIN conc_time_values c on s.id = c.fk_series_id")
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_time dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = prep_normalization(x=raw, newcols=c())
  out$raw = out$raw %>% mutate(time_hr=as.numeric(NA))
  #Missing time values
  out = check_missing(x=out, miss_col = "time_original", f=f, flag=TRUE)
  
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="time_units_original")
  #Normalize units
  out$raw$time_units_original = normalize_time_units(out$raw$time_units_original)
  #Non-numerics
  out = check_non_numeric(x=out, f=f, col="time_original")
  #Prep for conversion
  out$convert_ready = out$raw %>% mutate(time_hr = as.numeric(time_original))
  out$raw = NULL
  #Convert time
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="time_hr", 
                                          units="time_units_original", desired="hr",
                                          overwrite_units = FALSE)
  }
  #Convert Failed
  out = check_convert_failed(x=out, f=f, col="time_hr")
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("convert_ready")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("time_hr")) %>%
        return()
    }
  })
  return(out %>% bind_rows() %>% arrange(tempID))
}

normalize_time_units <- function(x){
  #Convert time
  x = tolower(x)
  conv = list(s=list("s", "sec", "second", "seconds"),
              min=list("min", "minute", "minutes"),
              hr=list("hr","hour", "hours", "h"),
              day=list("day", "days"),
              week=list("week", "weeks", "wk", "wks")
  )
  
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
