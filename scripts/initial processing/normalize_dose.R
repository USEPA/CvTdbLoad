
#' @description Doses are stored in mg/kg. In cases where the mass of the subject was not reported, and the dose was
#' administered as a simple mass and not a body weight proportion, a mg/kg dose was calculated using the average
#' mass for all subjects of that type.
#'@param raw A dataframe of weight information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_dose <- function(raw, f){
  message("...normalizing dose...")
  #message("Normalize_dose is still in development...")
  tmp = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Series %>%
      left_join(s_list$Studies, by=c("fk_study_id"="id")) %>%
      left_join(s_list$Subjects, by=c("fk_subject_id"="id")) %>%
      select(species, subtype, weight, weight_units, height, height_units, #Needed for weight extrapolation
             test_substance_name, dose_level, dose_level_units, dose_volume, administration_route, dose_duration, dose_duration_units) %>%
      distinct() %>%
      mutate(file=f)
  }) %>%
    bind_rows()
  # tmp = normalize_weight(raw=tmp, f=f)
  # tmp = normalize_height(raw=tmp, f=f)
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
  #Remove species and "body weight" from units field
  out$raw$dose_level_units = gsub(paste0(c("body weight", "bw", "b.w.",
                                           paste0("/",out$raw$species %>% unique()),
                                           out$raw$species %>% unique()), collapse="|"), 
                                  "",
                                  out$raw$dose_level_units) %>%
    gsub(" per ", "/", .) %>% 
    trimws()
  #Remove parenthetical information from dose_level_normalized
  out$raw$dose_level_normalized = gsub("\\([^()]*\\)", "", out$raw$dose_level_normalized) %>% trimws()
  #Missing dose value
  out = check_missing(x=out, miss_col = "dose_level", f=f, flag=TRUE)

  #Missing units
  out = check_missing_units(x=out, f=f, units_col="dose_level_units")
  #Percentage units flag
  out$percentage = out$raw %>% filter(grepl("%|percent*", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$percentage$tempID)
  if(nrow(out$percentage)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_percentage")
  }
  #Concentration units flag
  out$concentration = out$raw %>% filter(grepl("/l|/ml|/L|/mL", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$concentration$tempID)
  if(nrow(out$concentration)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_concentration")
  }
  #Radioactive units flag
  out$radioactive = out$raw %>% filter(grepl("MBq|uCi", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$radioactive$tempID)
  if(nrow(out$radioactive)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_radioactive")
  }
  #Rate units flag
  out$rate_units = out$raw %>% filter(grepl("/hour|/day|/minute|/second|/hr|/min|/s", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$rate_units$tempID)
  if(nrow(out$rate_units)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_rate")
  }
  #Gas/Liquid units flag
  out$gas_liquid = out$raw %>% filter(grepl("gas|liquid", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$gas_liquid$tempID)
  if(nrow(out$gas_liquid)){
    log_CvT_doc_load(f=f, m="dose_conversion_gas_liquid")
  }
  #Surface area conversion needed
  out$surface_area_needed = out$raw %>% filter(grepl("/cm2|/cm\\^|/m\\^", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$surface_area_needed$tempID)
  if(nrow(out$surface_area_needed)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_surface_area")
  }
  #ppm/ppb conversion needed
  out$parts_per = out$raw %>% filter(grepl("ppm|ppb", dose_level_units))
  out$raw = out$raw %>% filter(!tempID %in% out$parts_per$tempID)
  if(nrow(out$parts_per)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_ppm_ppb")
  }
  #List of doses
  out = check_subject_list(x=out, f=f, col="dose_level_normalized")
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="dose_level_normalized", estimated=c())
  #Dose range
  out = check_unit_range(x=out, f=f, col="dose_level_normalized", estimated=c())
  #Ready for conversion
  out$conversion = out$raw %>% 
    mutate(dose_level_normalized = suppressWarnings(as.numeric(dose_level_normalized))) %>%
    filter(!is.na(dose_level_normalized), !is.nan(dose_level_normalized))
  out$raw = out$raw %>% filter(!tempID %in% out$conversion$tempID)
  #Check unhandled cases
  if(nrow(out$raw)){
    message("...Unhandled cases for dose: ", paste0(out$raw$dose_level_normalized %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_dose_normalize_case")
  }
  #Dose needs weight (doesn't have / units)
  out$need_per_weight = out$conversion %>% filter(!grepl("/|per", dose_level_units))
  out$conversion = out$conversion %>% filter(!tempID %in% out$need_per_weight$tempID)
  #Fix need_per_weight - convert to mg then divide by kg weight (given or extrapolated)
  if(nrow(out$need_per_weight)){
    for(i in seq_len(nrow(out$need_per_weight))){
      out$need_per_weight[i,] = convert_units(x=out$need_per_weight[i,], 
                                            num="dose_level_normalized", 
                                            units="dose_level_units", desired="mg",
                                            overwrite_units = FALSE)
    }
    #Divide by weight (given or extrapolated)
    out$need_per_weight$dose_level_normalized = out$need_per_weight$dose_level_normalized / out$need_per_weight$weight_kg
  }
 
  #Convert dosages
  out$convert_ready = bind_rows(out$conversion, out$ci, out$unit_range)
  out$conversion = NULL; out$ci = NULL; out$unit_range = NULL
  for(i in seq_len(nrow(out$convert_ready))){
    #Molecular Weight conversion (have to find MW first)
    MW=NA
    if(grepl("mol/", out$convert_ready[i,]$dose_level_units)){
      MW = tryCatch({httk::get_physchem_param("MW", chem.name=tolower(out$convert_ready[i,]$test_substance_name))},
                    error=function(cond){NA})
      
    }
    #NEED subject weight to convert!
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="dose_level_normalized",
                                          units="dose_level_units", 
                                          desired="mg/kg",
                                          overwrite_units = FALSE,
                                          MW=MW)
  }
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("unit_range", "need_per_weight", "convert_ready")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("dose_level_normalized")) %>%
        return()
    }
  })
  #Convert dose_level_normalized to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% mutate(dose_level_normalized = suppressWarnings(as.numeric(dose_level_normalized)))
  })
  return(out %>% bind_rows() %>% arrange(tempID) %>% select(-tempID))
}
