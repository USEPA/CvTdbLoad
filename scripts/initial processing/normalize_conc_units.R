#'@description A helper function to normalize concentration units.
#'@param raw A dataframe of weight information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_conc <- function(raw, f){
  message("...normalizing conc...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     left_join(s_list$Subjects %>%
  #                 select(id, species), by=c("fk_study_id"="id")) %>%
  #     left_join(s_list$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
  #     select(id, species, conc_medium, analyte_name, analyte_name_secondary, analyte_casrn,
  #            conc_original=conc, conc_units_original=conc_units,
  #            conc_sd_original=conc_sd, conc_lower_bound_original=conc_lower_bound,
  #            conc_upper_bound_original=conc_upper_bound) %>%
  #     mutate(doc = f) %>%
  #     distinct()
  # }) %>%
  #   bind_rows()
  # tmp$species = normalize_species(x=tmp$species)
  # tmp = normalize_conc_medium(raw=tmp, f=f)
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_conc dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = prep_normalization(x=raw, newcols=c())
  #Fix missing conc - https://stackoverflow.com/questions/25768305/r-replace-multiple-values-in-multiple-columns-of-dataframes-with-na
  conc_cols = c("conc_original", "conc_sd_original", 
                "conc_lower_bound_original", "conc_upper_bound_original")
  out$raw[conc_cols] <- lapply(out$raw[conc_cols], 
                               function(x) replace(x, x %in% c("missing", "NA", "n/a"), NA))
  #Remove evaluation symbols
  out$raw = out$raw %>%
    mutate(conc_original=gsub(">|<|at least", "", conc_original),
           #Remove conc_medium tags in units
           conc_units_original = gsub(paste0(unique(out$raw$conc_medium), collapse="|"), "", 
                                      conc_units_original) %>% trimws(.))
  
  #Create normalization columns                                                                       
  out$raw = out$raw %>% mutate(conc=conc_original,
                               conc_sd=conc_sd_original,
                               conc_lower_bound=conc_lower_bound_original,
                               conc_upper_bound=conc_upper_bound_original)
  #Missing concentration values
  out$missing_conc = out$raw %>% filter(is.na(conc_original))
  out$raw = out$raw %>% filter(!tempID %in% unique(out$missing_conc$tempID))
  if(nrow(out$missing_conc)){
    log_CvT_doc_load(f=f, m="missing_conc_values")
  }
  #Get ND and NQ concentrations
  out$ND = out$raw %>% filter(conc_original %in% c("ND", "NQ"))
  out$raw = out$raw %>% filter(!tempID %in% unique(out$ND$tempID))
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="conc_units_original")
  #Normalize units
  out$raw$conc_units_original = normalize_conc_units(out$raw$conc_units_original)
  #Percentage units flag
  out$percentage = out$raw %>% filter(grepl("%|percent*", conc_units_original))
  out$raw = out$raw %>% filter(!tempID %in% out$percentage$tempID)
  if(nrow(out$percentage)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_percentage")
  }
  #Radioactive units flag
  out$radioactive = out$raw %>% filter(grepl("MBq|bq/", conc_units_original))
  out$raw = out$raw %>% filter(!tempID %in% out$radioactive$tempID)
  if(nrow(out$radioactive)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_radioactive")
  }
  #Rate units flag
  out$rate_units = out$raw %>% filter(grepl("/hour|/day|/minute|/second|/hr|/min|/s|/h|*h/", conc_units_original))
  out$raw = out$raw %>% filter(!tempID %in% out$rate_units$tempID)
  if(nrow(out$rate_units)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_rate")
  }
  #Non-numerics
  out = check_non_numeric(x=out, f=f, col="conc_original")
  #Prep for conversion
  out$convert_ready = out$raw %>% mutate(conc = as.numeric(conc_original))
  out$raw = NULL

  #Conc needs liquid portion (doesn't have / units)
  out$need_per_liquid = out$convert_ready %>% filter(!grepl("/|per|ppm|ppb|ppmv|ppbv", conc_units_original))
  out$convert_ready = out$convert_ready %>% filter(!tempID %in% out$need_per_liquid$tempID)
  #Is per weight
  out$per_weight = out$convert_ready %>% 
    filter(grepl("/kg|/g|/mg|/ng|/ug", conc_units_original)) %>%
    convert_mass_per_mass()
  #Try to normalize
  #Filter out ones that didn't get normalized
  #Add them back to the convert_ready list
  #Filer to convert_ready and convert to numeric
  out$convert_ready = out$convert_ready %>% filter(!tempID %in% out$per_weight$tempID) %>%
    mutate(across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), as.numeric))
  #remaining logic TBD
  #message("...Conc_units conversion logic TBD")
  #warning("...Conc_units conversion logic TBD")
  #Need to split up between routes as well (ug/mL tissue and ug/m3 breath)
  for(t in c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")){
    for(i in seq_len(nrow(out$convert_ready))){
      #Molecular Weight conversion (have to find MW first)
      MW=NA
      if(grepl("mol/", out$convert_ready[i,]$conc_units_original)){
        MW = tryCatch({httk::get_physchem_param("MW", chem.name=tolower(out$convert_ready[i,]$analyte_name))},
                      error=function(cond){NA})
        
      }
      out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                            num=t, 
                                            units="conc_units_original", desired="ug/ml",
                                            overwrite_units = FALSE,
                                            MW=MW)
    }
  }
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("ND", "convert_ready", "per_weight")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("conc", "conc_sd", 
                                              "conc_lower_bound", "conc_upper_bound")) %>%
        return()
    }
  })
  out = lapply(out, function(n){
    tmp = n
    #Check for negative values
    if(any(tmp$conc < 0, na.rm=TRUE) | any(tmp$conc_lower_bound < 0, na.rm=TRUE) | 
       any(tmp$conc_upper_bound < 0, na.rm=TRUE) | any(tmp$conc_sd < 0, na.rm=TRUE)){
      log_CvT_doc_load(f=f, m="negative_conc_values")
    }
    #Convert all back to character to maintain non_numeric columns that weren't converted
    return(tmp %>% mutate(across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), suppressWarnings(as.character))))
  })
  #Recombine
  out = out %>%
    bind_rows() %>% 
    arrange(tempID)
  return(out)
}

#'@description Helpfer function to normalize concentration units
normalize_conc_units <- function(x){
  if(length(x)==0){
    return(x)
  }
  #Convert conc units
  x = tolower(x)
  conv = list(`ng/ml`=list("ng/ml", "ng/mL"),
              `mg/ml`=list("mg/ml"),
              `percentage`=list("%", "% of original dose", "percent", "% Dose", "% dose", "cumulative %"),
              `ug/g`=list("ug/g", "ug/g liver"),
              `ug/kg`=list("ug/kg"),
              `mg/kg`=list("mg/kg"),
              `ug/ml`=list("mcg/mL", "mcg/ml", "ug/ml"),
              `mg/l`=list("mg/l", "mg/L"),
              `ug/l`=list("ug/l", "ug/L"),
              `ug/ml`=list("ug/ml", "ug/mL"),
              `ng/l`=list("ng/l", "ng/L"),
              `pmol/ml`=list("pmole/ml"),
              ug=list("ug", "ug", "ug concentration equivalents", "?g"),
              mg=list("mg"))
  
  x = lapply(x, function(s){
    for(c in names(conv)){
      if(!s %in% conv[[c]]){
        if(grepl(paste0(conv[[c]], collapse="|"), s)){
          #message("Potential match for: ", s)
          return(c)
        }
        next
      } else {
        return(c)  
      }
    }
    return(s)
  }) %>% unlist()
  return(x)
}

#'@description Helpfer function to help convert mass/mass concentrations
convert_mass_per_mass <- function(x){
  #Filter density table to matching conc_medium and species
  density_table = readxl::read_xlsx("input/httk_tissue_density.xlsx") %>%
    select(-Reference) %>%
    mutate(Tissue = tolower(Tissue)) %>%
    dplyr::rename(value = `Density (g/cm^3)`)
  # density_table = httk::tissue.data %>%
  #   mutate(across(c("Species", "Tissue"), tolower)) %>%
  #   filter(grepl("Vol", variable),
  #          Species %in% unique(x$species),
  #          Tissue %in% unique(x$conc_medium) )%>% 
  #   select(-Reference, -variable)
  
  x = x %>%
    left_join(density_table , by=c("conc_medium"="Tissue")) %>%
    mutate(across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), as.numeric))
  
  for(t in c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")){
    for(i in seq_len(nrow(x))){
      if(!is.na(x$value[i])){
        x[i,] = convert_units(x=x[i,], 
                              num=t, 
                              units="conc_units_original", desired="ug/ml",
                              overwrite_units = FALSE,
                              MW=(x$value[i])) #g/mL
        
      }
      
    }
  }
  return(x %>% select(-value))
}
