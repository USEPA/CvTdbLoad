
normalize_age <- function(raw, f){
  message("...normalizing age...")
  age_dict = readxl::read_xlsx("input/dictionaries/age_category_dict.xlsx")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Subjects %>% select(species, age, age_category, age_units) %>% distinct() %>%
  #     mutate(doc = f)
  # }) %>%
  #   bind_rows()
  # tmp$species = normalize_species(tmp$species)
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_age dataframe empty...returning...")
    return(raw)
  }
  
  #Prep list for dataframe subsets
  out = list()
  out$raw = prep_normalization(x=raw, newcols=c())
  out$raw$species = tolower(trimws(out$raw$species))
  out$raw = out$raw %>% mutate(age_normalized = age)
  #Extract units
  #out$raw = extract_weight_units(x=out$raw)
  #Cases where units were extracted into age_category column
  out$raw$age_units[is.na(out$raw$age_units)] = out$raw$age_category[is.na(out$raw$age_units)]
  out$raw = extract_units(x=out$raw, units_col="age_units", conv_col="age_normalized", unit_type="age")
  out$raw$age_units_original = out$raw$age_units
  #Extrapolate age
  out = norm_extrapolate(x=out, f=f, extrap_type="age")
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="age_units")
  out$missing_units$age_units = NA #reverting missing units back to NA
  out$unmatched_species = out$raw %>% filter(!species %in% age_dict$species)
  if(nrow(out$unmatched_species)){
    out$unmatched_species$age_category = "unmatched_species"  
  }
  #Don't flag, just skip the species without matches
  # if(nrow(out$unmatched_species)){
  #   message("...Missing age category species match...Needs further curation - ", 
  #           paste0(out$unmatched_species$species, collapse = "; "))
  #   log_CvT_doc_load(f=f, m="no_species_age_category_match")
  # }
  out$raw = out$raw %>% filter(!tempID %in% out$unmatched_species$tempID)
  #Normalize units
  out$raw$age_units = normalize_age_units(out$raw$age_units)
  #Remove extraneous characters
  out$raw = out$raw %>%
    mutate(age_normalized = sub("mean=|old|between|GD|gestational|≥|avg", "", age_normalized) %>%
             trimws(),
           age_units = sub("gestational", "", age_units) %>% trimws(.))
  #List of ages
  out = check_subject_list(x=out, f=f, col="age_normalized")
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="age_normalized", estimated=c())
  #age range
  out = check_unit_range(x=out, f=f, col="age_normalized", estimated=c())
  #Missed cases to handle or curate
  out$need_curation = out$raw %>%
    mutate(age_num = suppressWarnings(as.numeric(age_normalized))) %>%
    filter(is.na(age_num) | (is.na(age_num & is.na(age_category)))) %>%
    select(-age_num)
  
  if(nrow(out$need_curation)){
    message("...Unhandled age normalization...Needs further curation")
    log_CvT_doc_load(f=f, m="unhandled_age_normalize_case")
  }
  out$raw = out$raw %>% filter(!tempID %in% out$need_curation$tempID) %>%
    mutate(age_normalized = as.numeric(age_normalized))
  out$to_convert = out$raw
  out$raw = NULL
  #Ready for conversion
  #Combine and convert prepped datasets
  out$mapped_age = bind_rows(out$ci, out$unit_range, out$to_convert)
  #Remove mapped dataframes
  out$ci = NULL; out$unit_range = NULL; out$to_convert = NULL
  if(nrow(out$mapped_age)){
    out$mapped_age = map_age_category(x=out$mapped_age, dict=age_dict)
  }
  out$matching_err = out$mapped_age %>% filter(!tempID %in% out$mapped_age$tempID[out$mapped_age$age_category %in% names(age_dict)])
  out$mapped_age = out$mapped_age %>% filter(!tempID %in% out$matching_err$tempID)
  if(nrow(out$matching_err)){
    m = out$matching_err$age_category %>% unique() %>% unlist()
    message("...Age category matching error: ", paste0(m, collapse=", "))
    log_CvT_doc_load(f=f, m="age_category_match_error")
  }
  
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("extrapolate", "mapped_age")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("age_normalized", "age_category")) %>%
        return()
    }
  }) %T>% { names(.) <- names(out) }
  #Convert age_normalized to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% mutate(age_normalized = suppressWarnings(as.numeric(age_normalized)))
  })
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  out %>% 
    bind_rows() %>% 
    dplyr::rename(age_units_normalized = age_units) %>%
    arrange(tempID) %>% 
    select(-tempID) %>% 
    return()
  #Match to age category
  #Values represent the lower threshold for age inclusion in this category
  #Younger than infant was categorized "neonate"
}

map_age_category <- function(x, dict){
  for(i in seq_len(nrow(x))){
    m_dict = dict %>% filter(species == x$species[i]) %>% select(-species)
    if(!grepl(m_dict$unit, x$age_units[i])){
      #Convert units to desired
      x[i,] = convert_units(x[i,], num="age_normalized", units="age_units", 
                            desired=m_dict$unit, overwrite_units = TRUE)
      if(is.na(x$age_normalized[i])){
        x$age_category[i] = "unhandled_age_unit_conversion"
        next
      }
    } 
    if(x$age_normalized[i] > (2 * m_dict$aged)){ #impossible ages > 2 * max category
      x$age_category[i] = "impossible_subject_age"
      next
    } else if(x$age_normalized[i] < m_dict$infant){ #neonate if less than infant category
      x$age_category[i] = "neonate"
      next
    }
    #Find the first case where the age is > the category checked (in reverse order)
    x$age_category[i] = rev(names(m_dict %>% select(-unit)))[(which(x$age_normalized[i] >= rev(m_dict %>% select(-unit)))[1])]
  }
  return(x)
}

normalize_age_units <- function(x){
  #Convert time
  x = tolower(x)
  conv = list(s=list("s", "sec", "second", "seconds"),
              min=list("min", "minute", "minutes"),
              hr=list("hr","hour", "hours", "h"),
              day=list("day", "days"),
              week=list("week", "weeks", "wk", "wks"),
              month=list("month", "months")
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

#Deprecated function
# extract_age_units <- function(x){
#   out_units = list()
#   #Has age_units field
#   out_units$has_units = x %>% filter(!is.na(age_units))
#   x = x %>% filter(!tempID %in% out_units$has_units$tempID)
#   #NA age_units and no units in age_category
#   out_units$missing_units = x %>% filter((!grepl("week|wks|year|yr|day|GD|gestation", age_category)),
#                                          !grepl("week|wks|year|yr|day|GD|gestation", age))
#   out_units$missing_units$age_category = "missing_units"
#   x = x %>% filter(!tempID %in% out_units$missing_units$tempID)
#   out_units$age_units = x %>% filter(grepl("week|wks|year|yr|day|GD|gestation", age))
#   x = x %>% filter(!tempID %in% out_units$age_units$tempID)
#   out_units$age_cat_units = x %>% filter(grepl("week|wks|year|yr|day|GD|gestation", age_category))
#   x = x %>% filter(!tempID %in% out_units$age_cat_units$tempID)
#   
#   if(nrow(out_units$age_units)){
#     out_units$age_units$age_category = lapply(seq_len(nrow(out_units$age_units)), function(i){
#       ifelse(grepl("week|wks", out_units$age_units$age[i]), "week",
#              ifelse(grepl("year|yr", out_units$age_units$age[i]), "year",
#                     ifelse(grepl("day|GD|gestation", out_units$age_units$age[i]), "day",
#                            ifelse(grepl("month", out_units$age_units$age[i]), "month", "missing_units")))
#       )
#     }) %>% unlist()
#     #Remove age units from age
#     out_units$age_units = out_units$age_units %>%
#       mutate(age_normalized = gsub("week|weeks|wk|wks|years|year|-year|yr|yrs|day|days|GD|gestational day|gestational days", "", age))
#   }
#   
#   if(nrow(out_units$age_cat_units)){
#     out_units$age_cat_units$age_category = lapply(1:nrow(out_units$age_cat_units), function(i){
#       ifelse(grepl("week|wks", out_units$age_cat_units$age_category[i]), "week",
#              ifelse(grepl("year|yr", out_units$age_cat_units$age_category[i]), "year",
#                     ifelse(grepl("day|GD|gestation", out_units$age_cat_units$age_category[i]), "day",
#                            ifelse(grepl("month", out_units$age_cat_units$age_category[i]), "month", NA)))
#       )
#     }) %>% unlist()
#   }
#   #Remove empty list elements
#   out_units = out_units[sapply(out_units, nrow) > 0]
#   return(out_units %>% bind_rows())
# }