#'@description Helper function to add temp ID column and new empty columns before normalization.
#'@param x Input dataframe being prepped for normalization
#'@param newcols
#'
#'@return Modified version of the input `x` parameter
normalization_prep <- function(x, newcols){
  #Add unique identifier to help filter out select weight types
  x$tempID = seq_len(nrow(x))
  #Add empty column
  x[, newcols] = NA
  return(x)
}

#Eventually make this a generic function for all norm extrapolation processes...
#'@description Helper function to check for missing metric and attemp to exrapolate values.
#'@param x Input list of datasets being normalized
#'@param f Filename for flagging purposes
#'@param extrap_type The type of extrapolation being performed (weight is the only accepted form at this time).
#'
#'@return Modified version of the input `x` parameter
norm_extrapolate <- function(x, f, extrap_type){
  #weight Group that needs extrapolation based on similar species/subtype
  x$extrapolate = x$raw %>% filter(is.na(!!as.symbol(extrap_type)))
  x$raw = x$raw %>% filter(!tempID %in% x$extrapolate$tempID)
  
  #Extrapolate weights
  if(nrow(x$extrapolate)){
    if(extrap_type == "weight"){
      message("...extrapolating ", extrap_type)
      #Extrapolate
      #Average of species and subtype match or just species
      map_spec_sub = query_cvt("SELECT DISTINCT species, subtype, weight_kg FROM cvt.subjects") %>%
        mutate(across(c(species, subtype), ~tolower(trimws(.)))) %>%
        group_by(species, subtype) %>%
        summarise(avg_weight_kg = mean(weight_kg, na.rm=TRUE))
      map_spec = query_cvt("SELECT DISTINCT species, subtype, weight_kg FROM cvt.subjects") %>%
        mutate(across(c(species, subtype), ~tolower(trimws(.)))) %>%
        group_by(species) %>%
        summarise(avg_weight_kg = mean(weight_kg, na.rm=TRUE))
      #20
      x$extrapolate = x$extrapolate %>% mutate(across(c(species, subtype), ~tolower(trimws(.))))
      x$extrap_spec_sub = x$extrapolate %>%
        select(-weight_kg) %>%
        left_join(map_spec_sub, by=c("species", "subtype")) %>%
        filter(!is.na(avg_weight_kg)) %>%
        distinct() %>%
        mutate(weight_estimated = 1) %>%
        dplyr::rename(weight_kg = avg_weight_kg)
      x$extrapolate = x$extrapolate %>% filter(!tempID %in% x$extrap_spec_sub$tempID)
      x$extrap_spec = x$extrapolate %>%
        select(-weight_kg) %>%
        left_join(map_spec, by=c("species")) %>%
        filter(!is.na(avg_weight_kg)) %>%
        distinct() %>%
        mutate(weight_estimated = 1) %>%
        dplyr::rename(weight_kg = avg_weight_kg)
      x$extrapolate = x$extrapolate %>% filter(!tempID %in% x$extrap_spec$tempID)
      #Always end up with a weight of some kind, then weight_bool it
      if(nrow(x$extrapolate)){
        message(paste0("...Unhandled extrapolation cases for: ", extrap_type))
        log_CvT_doc_load(f=f, m=paste0(extrap_type,"_extrapolation_attempt_failed"))
      }
    }
  }
  return(x)
}

#'@description Helper function to check for missing values for desired column.
#'@param x Input list of datasets being normalized
#'@param miss_col The column to check
#'@param f Filename for flagging purposes
#'@param flag Whether to log a flag for the missing column
#'
#'@return Modified version of the input `x` parameter
check_missing <- function(x, miss_col, f, flag=FALSE){
  x$missing = x$raw %>% filter(!!as.symbol(miss_col) %in% c("NA", "n/a", "N/A")  |
                                       is.na(!!as.symbol(miss_col)))
  x$raw = x$raw %>% filter(!tempID %in% x$missing$tempID)
  if(flag & nrow(x$missing)){
    message("...Needs further curation: Missing - ", miss_col)
    log_CvT_doc_load(f=f, m=paste0("missing_",miss_col,"_values"))
  }
  return(x)
}

#'@description Helper function to check for missing units for desired metric column.
#'@param x Input list of datasets being normalized
#'@param f Filename for flagging purposes
#'@param units_col The units column to check
#'
#'@return Modified version of the input `x` parameter
check_missing_units <- function(x, f, units_col){
  x$missing_units = x$raw %>% filter(!!as.symbol(units_col) %in% c("missing_units", "NA", "n/a", "N/A")  |
                                       is.na(!!as.symbol(units_col)))
  if(nrow(x$missing_units)){
    message("...Needs further curation: Missing - ", units_col)
    log_CvT_doc_load(f=f, m=paste0("curation_needed_",units_col,"_units"))
  }
  x$raw = x$raw %>% filter(!tempID %in% x$missing_units$tempID)
  return(x)
}

#'@description Helper function to check if a metric entry has a ; separated list, therefore should be split.
#'@param x Input list of datasets being normalized
#'@param f Filename for flagging purposes
#'@param col The column being checked/normalized
#'
#'@return Modified version of the input `x` parameter
check_subject_list <- function(x,f, col){
  #List of weights
  x$split_subject = x$raw %>% filter(grepl(";|, ", !!as.symbol(col)))
  if(nrow(x$split_subject)){
    message("...Needs further curation: ", paste0(x$split_subject[[col]], collapse=";"))
    log_CvT_doc_load(f=f, m="curation_needed_split_subject")
  }
  x$raw = x$raw %>% filter(!tempID %in% x$split_subject$tempID)
  return(x)
}

#'@description Helper function to check for confidence intervals for a metric, and handling them.
#'@param x Input list of datasets being normalized
#'@param f Filename for flagging purposes
#'@param col The column being checked/normalized
#'@param estimated The column for *_estimated flags (0, 1, 2)
#'
#'@return Modified version of the input `x` parameter
check_unit_ci <- function(x, f, col, estimated){
  #Removed |? at end of regex, not sure what it was used for??
  x$ci = x$raw %>% filter(grepl("±|\\+/-|\\+", !!as.symbol(col)))
  x$raw = x$raw %>% filter(!tempID %in% x$ci$tempID)
  
  if(nrow(x$ci)){
    x$ci = x$ci %>%
      mutate(across(.cols=all_of(col), .fns = ~sub('±.*|\\+/-.*|\\+.*', '', !!as.symbol(col))))
    tryCatch({ x$ci %>% mutate(across(.cols=all_of(col), .fns = ~as.numeric(gsub(",", "", !!as.symbol(col))))) },
             warning = function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"))
             })
    x$ci = x$ci %>% mutate(across(.cols=all_of(col), .fns = ~as.numeric(gsub(",", "", !!as.symbol(col)))))
    if(length(estimated)){
      x$ci[[estimated]] <- 0
    }
  } else {
    x$ci = x$ci %>% mutate(across(.cols=all_of(col), .fns = ~suppressWarnings(as.numeric(!!as.symbol(col)))))
  }
  return(x)
}

#'@description Helper function to check for ranges, and handling them
#'@param x Input list of datasets being normalized
#'@param f Filename for flagging purposes
#'@param col The column being checked/normalized
#'@param estimated The column for *_estimated flags (0, 1, 2)
#'
#'@return Modified version of the input `x` parameter
check_unit_range <- function(x, f, col, estimated){
  x$unit_range = x$raw %>% filter(grepl("[\\-]|to|\\-|-|-|and|or", !!as.symbol(col)))
  x$raw = x$raw %>% filter(!tempID %in% x$unit_range$tempID)
  
  #Normalize weight groups
  if(nrow(x$unit_range)){
    x$unit_range = x$unit_range %>% 
      #mutate(tmp = gsub("kg|kilograms|kilo|kilogram", "", weight)) %>%
      tidyr::separate(!!as.symbol(col), c("lower", "upper"), sep="[\\-]|to|\\-|-|-|and|or", remove=FALSE)
    #Check for numeric conversion issues
    tryCatch({as.numeric(gsub(",", "", x$unit_range$lower))},
             warning=function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"))
             })
    tryCatch({as.numeric(gsub(",", "", x$unit_range$upper))},
             warning=function(cond){
               log_CvT_doc_load(f=f, m=paste0(col, "_numeric_conversion_NA"))
             })
    x$unit_range = x$unit_range %>%
      mutate(lower = as.numeric(gsub(",", "", lower)), #Remove "," place separator
             upper = as.numeric(gsub(",", "", upper))) %>%
      rowwise() %>% 
      mutate(across(.cols=all_of(col), .fns = ~mean(c(upper, lower), na.rm=T))) %>%
      select(-upper, -lower)
    if(length(estimated)){
      x$unit_range[[estimated]] = 2
    }
  } else {
    x$unit_range = x$unit_range %>% 
      mutate(across(.cols=all_of(col), .fns = ~suppressWarnings(as.numeric(!!as.symbol(col)))))
  }
  
  return(x)
}

check_non_numeric <- function(x, f, col){
  x$non_numeric = x$raw %>%
    mutate(non_numeric_check = suppressWarnings(as.numeric(gsub(",", "", !!as.symbol(col))))) %>%
    filter(is.na(non_numeric_check)) %>%
    select(-non_numeric_check)
  if(nrow(x$non_numeric)){
    message("...Non-numeric ", col," value found...need to handle...")
    log_CvT_doc_load(f=f, m=paste0("unhandled_cvt_",col,"_non_numeric"))
  }
  x$raw = x$raw %>% filter(!tempID %in% x$non_numeric$tempID)
  return(x)
}

check_convert_failed <- function(x, f, col){
  x$convert_failed = x$convert_ready %>% filter(is.na(!!as.symbol(col)))
  if(nrow(x$convert_failed)){
    message("...",col," conversion failed...")
    log_CvT_doc_load(f=f, m=paste0("cvt_",col,"_convert_fail"))
  }
  x$convert_ready = x$convert_ready %>% filter(!tempID %in% x$convert_failed$tempID)
  return(x)
}

normalize_boolean <- function(x, col){
  #If multiple columns are provided
  
  for(a in col){
    #Convert column of missing values (imported as logical) to character columns
    if(all(is.na(x[[a]])) & typeof(x[[a]]) == "logical"){
      x[[a]] = as.character(x[[a]])
    }
    x[(!is.na(x[[a]]) & x[[a]] != "0" & x[[a]] != 0), a] <- "1"
    x[is.na(x[[a]]), a] <- "0"
    x[[a]] = as.numeric(x[[a]])
  }
  return(x)
}

check_radiolabel <- function(raw, f){
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     select(analyte_name, analyte_name_secondary, fk_study_id, radiolabeled) %>%
  #     left_join(s_list$Studies %>% select(id, test_substance_name), by=c("fk_study_id"="id")) %>%
  #     return()
  # }) %>%
  #   bind_rows()
  check = raw %>%
    filter((is.na(radiolabeled) | radiolabeled != 1),
           !grepl("DTXSID", analyte_name),
           !grepl("DTXSID", analyte_name_secondary),
           !grepl("DTXSID", test_substance_name)) %>%
    mutate(analyte_name_check = grepl("([1-9][0-9])", analyte_name),
           analyte_name_secondary_check = grepl("([1-9][0-9])", analyte_name_secondary),
           test_substance_name_check = grepl("([1-9][0-9])", test_substance_name)) %>%
    #If any of the chemical name fields contained 2 digit values
    filter(analyte_name_check == TRUE | 
             analyte_name_secondary_check == TRUE | 
             test_substance_name_check == TRUE)
  if(nrow(check)){
    message("...chemicals or analyte found that need radiolabel set to 1")
    log_CvT_doc_load(f=f, m=paste0("potential_missing_radiolabel_detected"))
  }
}

normalize_conc_medium <- function(raw, f){
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series# %>% select(id, conc_medium) %>% mutate(doc = f)
  # }) %>%
  #   bind_rows()
  #Concentration Normalization Dictionary
  conc_dict = readxl::read_xlsx("input/dictionaries/conc_medium_dict.xlsx") %>%
    mutate(conc_medium_original = tolower(conc_medium_original)) %>%
    dplyr::rename(conc_medium_id = id) %>%
    select(-units)
  #Match conc_medium
  out = raw %>%
    mutate(conc_medium_original = trimws(tolower(conc_medium))) %>%
    left_join(conc_dict, by="conc_medium_original")
  #Check for unmatched conc_medium
  unmatched = out %>%
    filter(is.na(conc_medium_normalized))
  #Flag unmatched
  if(nrow(unmatched)){
    message("...conc_meduium need curation: ", paste0(unique(unmatched$conc_medium_original), collapse="; "))
    log_CvT_doc_load(f=f, m="curate_conc_medium")
  }
  return(out)
}

#'@description Function to check if processed document is missing required fields.
#'@param df List of dataframes for the sheets within an extraction template
#'@param f Filename for flagging purposes
#'
#'@return None. Logs any flags
check_required_fields <- function(df, f){
  for(t in names(df)){
    req_fields = switch(t,
                        "Documents" = c("id", "document_type"),
                        "Studies" = c("id", "dose_level", #"dose_vehicle", 
                                      "administration_route_normalized"),
                        "Subjects" = c("id", "weight_kg", "species"),
                        "Series" = c("id", "radiolabeled", "conc_units", "conc_medium_normalized",
                                     "fk_study_id", "fk_subject_id"),
                        "Conc_Time_Values" = c("fk_series_id", "time_hr")
    )  
    #Check if missing required field entirely
    if(any(!req_fields %in% names(df[[t]]))){
      message("Required field missing: ", req_fields[!req_fields %in% names(df[[t]])])
      #Flag missing fields
      for(field in req_fields[!req_fields %in% names(df[[t]])]){
        #message(paste0("missing_required_field_", field))
        log_CvT_doc_load(f=f, m=paste0("missing_required_field_", field))
      }
    }
    
    #Flag empty required fields
    for(field in req_fields[req_fields %in% names(df[[t]])]){
      if(any(is.na(df[[t]][field]))){
        #message(paste0("NA_in_required_field_", field))
        log_CvT_doc_load(f=f, m=paste0("NA_in_required_field_", field))
      }
    }
    
    #Special case check
    if(t == "Documents"){
      if(any(is.na(df[[t]]$pmid) & is.na(df[[t]]$other_study_identifier))){
        log_CvT_doc_load(f=f, m="missing_document_identifier")
      }
    }
    if(t == "Studies"){
      #Check chemical information (each row needs something in test_substance_name, test_substance_name_secondary, test_substance_casrn)
      tmp = lapply(seq_len(nrow(df[[t]])), function(r){
        any(!is.na(df[[t]]$test_substance_name[r]), !is.na(df[[t]]$test_substance_name_secondary[r]), !is.na(df[[t]]$test_substance_casrn[r]))
      }) %>% unlist()
      if(any(tmp == FALSE)){
        log_CvT_doc_load(f=f, m="missing_required_test_substance_chemical_info")
      }
      
      if(all(c("dose_duration", "dose_duration_units") %in% names(df[[t]]))){
        #Check if dose_duration present (if dose_frequency present)
        #if the dose frequency is "1" i guess you wouldn't need a dose duration - Risa Sayre 2021-12-1
        tmp = df[[t]] %>%
          filter(dose_frequency != 1)
        if(any(!is.na(tmp$dose_frequency)) & all(is.na(tmp$dose_duration))){
          log_CvT_doc_load(f=f, m="dose_frequency_present_without_dose_duration")
        } 
      }
      if(all(c("dose_duration", "dose_duration_units") %in% names(df[[t]]))){
        #Check if dose_duration_units present (if dose_duration present)
        if(any(!is.na(df[[t]]$dose_duration)) & all(is.na(df[[t]]$dose_duration_units))){
          log_CvT_doc_load(f=f, m="missing_dose_duration_units")
        }
      }
      if(all(c("administration_term", "administration_term_units") %in% names(df[[t]]))){
        #Check if administration_term_units present (if administration_term present)
        if(any(!is.na(df[[t]]$administration_term)) & all(is.na(df[[t]]$dose_duration_units))){
          log_CvT_doc_load(f=f, m="missing_administration_term_units")
        }
      }
        
      #dose duration is required for inhalation/dermal
      #If dose_frequency is not 1 or NULL, then we need a dose_duration
      #although, we have logic that if dose_frequency is present, they must have dose_duration...so simplifying
      tmp = df[[t]] %>% filter((administration_route_normalized %in% c("inhalation", "dermal") | grepl("infusion", administration_route_original)),
                               is.na(dose_duration) )
      if(nrow(tmp)){
        log_CvT_doc_load(f=f, m="missing_dose_duration_for_inhalation_dermal_iv_infusion_study")
      }
    }
    
    if(t == "Series"){
      #Check chemical information (each row needs something in analyte_name, analyte_name_secondary, analyte_casrn)
      tmp = lapply(seq_len(nrow(df[[t]])), function(r){
        any(!is.na(df[[t]]$analyte_name[r]), !is.na(df[[t]]$analyte_name_secondary[r]), !is.na(df[[t]]$analyte_casrn[r]))
      }) %>% unlist()
      if(any(tmp == FALSE)){
        log_CvT_doc_load(f=f, m="missing_required_test_substance_chemical_info")
      }
    }
  }
}

#'@description Function to check if any sheet of loaded template is empty
#'@param template List of loaded template sheets
#'
#'@return Boolean TRUE or FALSE if an empty sheet exists
check_empty_sheet <- function(template){
  if(purrr::is_empty(template)){
    return(TRUE)
  }
  for(s in template){
    if(!nrow(s)) return(TRUE)
  }
  return(FALSE)
}
