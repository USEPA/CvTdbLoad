
#' @description Extraction of units from dose_duration field (similar to height/weight)
#'@param raw A dataframe of weight information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_dose_duration <- function(raw, f){
  message("...normalizing dose...")
  #message("Normalize_dose is still in development...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     left_join(s_list$Studies, by=c("fk_study_id"="id")) %>%
  #     left_join(s_list$Subjects, by=c("fk_subject_id"="id")) %>%
  #     select(dose_duration, dose_duration_units) %>%
  #     distinct() %>%
  #     mutate(file=f)
  # 
  # }) %>%
  #   bind_rows()
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_dose dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c("dose_duration_units_normalized")) %>%
    mutate(dose_duration_units_normalized = gsub("within", "", dose_duration) %>%
             gsub("single|once|1|one", "1", .))
  #out$raw = extract_weight_units(x=out$raw)
  out$raw = extract_units(x=out$raw, units_col="dose_duration_units", 
                          conv_col="dose_duration_units_normalized", unit_type="dose_duration") %>%
    mutate(dose_duration_units_normalized = gsub("GD", "", dose_duration_units_normalized) %>%
             gsub("[[:space:]]", "", .))
  out$raw$dose_duration_units[out$raw$dose_duration_units == "missing_units"] = NA
  
  return(out %>% 
           bind_rows() %>% 
           arrange(tempID) %>% 
           select(dose_duration = dose_duration_units_normalized, dose_duration_units))
}
