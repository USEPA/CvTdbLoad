
#' @description Doses are stored in mg/kg. In cases where the mass of the subject was not reported, and the dose was
#' administered as a simple mass and not a body weight proportion, a mg/kg dose was calculated using the average
#' mass for all subjects of that type.
#'@param raw A dataframe of weight information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_dose <- function(raw){
  message("...normalizing dose...")
  message("Normalize_dose is still in development...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Studies %>% select(test_substance_name, dose_level, dose_level_units, dose_volume, administration_route) %>% distinct()
  # }) %>%
  #   bind_rows()
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_dose dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = prep_normalization(x=raw, newcols=c("dose_level_normalized"))
  #Set to convert column to maintain original
  out$raw$dose_level_normalized = out$raw$dose_level
  #out$raw$dose_level_units_original = out$raw$dose_level_units
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="dose_level_units")
  #List of doses
  out = check_subject_list(x=out, f=f, col="dose_level_normalized")
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="dose_level_normalized", estimated=c())
  #Dose range
  out = check_unit_range(x=out, f=f, col="dose_level_normalized", estimated=c())
  #Ready for conversion
  out$conversion = out$raw %>% 
    mutate(dose_level_normalized = suppressWarnings(as.numeric(dose_level_normalized))) %>%
    filter(!is.na(dose_level_normalized))
  out$raw = out$raw %>% filter(!tempID %in% out$conversion$tempID)
  #Check unhandled cases
  if(nrow(out$raw)){
    message("...Unhandled cases for dose: ", paste0(out$raw$dose_level_normalized %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_dose_normalize_case")
  }
  #out$unhandled_cases = out$raw
  #Convert dosages
  out$convert_ready = bind_rows(out$conversion, out$ci, out$weight_range)
  out$conversion = NULL; out$ci = NULL; out$weight_range = NULL
  for(i in seq_len(nrow(out$convert_ready))){
    #NEED subject weight to convert!
    message("...Dose conversion logic TBD...")
    warning("...Dose conversion logic TBD...")
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="dose_level_normalized", 
                                          units="dose_level_units", desired="mg/kg",
                                          overwrite_units = FALSE)
  }
  #Convert weight_kg to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% mutate(dose_level_normalized = suppressWarnings(as.numeric(dose_level_normalized)))
  })
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  return(out %>% bind_rows() %>% arrange(tempID) %>% select(-tempID))
}