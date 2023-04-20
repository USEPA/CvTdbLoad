#'@description A helper function to normalize subject weight (associated with dose normalization).
#'@param raw A dataframe of weight information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_weight <- function(raw, f){
  message("...normalizing weight...")
  #weight_estimated --> 0 = No estimation (just conversion); 1 = extrapolated; 2 = range mean
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Subjects %>% select(species, subtype, weight, weight_units) %>% distinct() %>%
  #     mutate(doc = f)
  # }) %>%
  #   bind_rows()
  # tmp$species = normalize_species(tmp$species)
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_weight dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c("weight_kg", "weight_estimated"))
  #Set to convert column to maintain original
  out$raw$weight_kg = out$raw$weight
  #Extract units
  #out$raw = extract_weight_units(x=out$raw)
  out$raw = extract_units(x=out$raw, units_col="weight_units", 
                          conv_col="weight_kg", unit_type="weight")
  #Extrapolate  weights
  out = norm_extrapolate(x=out, f=f, extrap_type = "weight")
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="weight_units")
  out$missing_units$weight_units = NA #Replacing missing units with NA after flagging
  #List of weights
  out = check_subject_list(x=out, f=f, col="weight_kg")
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="weight_kg", estimated="weight_estimated")
  #Weight range
  out = check_unit_range(x=out, f=f, col="weight_kg", estimated="weight_estimated")
  #Ready for conversion
  out$conversion = out$raw %>% 
    mutate(weight_kg = suppressWarnings(as.numeric(weight_kg))) %>%
    filter(!is.na(weight_kg)) %>%
    mutate(weight_estimated = 0)
  out$raw = out$raw %>% filter(!tempID %in% out$conversion$tempID)
  
  if(nrow(out$raw)){
    message("...Unhandled cases for weight: ", paste0(out$raw$weight_kg %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_weight_normalize_case")
  }
  #out$unhandled_cases = out$raw
  #Convert kg, g, mg, lbs, etc.
  out$convert_ready = bind_rows(out$conversion, out$ci, out$unit_range)
  out$conversion = NULL; out$ci = NULL; out$unit_range = NULL
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="weight_kg", 
                                          units="weight_units", desired="kg",
                                          overwrite_units = FALSE)
  }
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("extrapolate", "extrap_spec_sub", "extrap_spec", "convert_ready", "unit_range")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("weight_kg")) %>%
        return()
    }
  })
  #Convert weight_kg to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% mutate(weight_kg = suppressWarnings(as.numeric(weight_kg)))
  })
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  out = out %>%
    bind_rows() %>% arrange(tempID) %>% select(-tempID)
  out$weight_units[out$weight_units == "missing_units"] = NA #Replace missing_units with NA after flagging
  return(out)
}

#Deprecated weight unit extraction function
# extract_weight_units <- function(x){
#   out_units = list()
#   #Has weight_units field
#   out_units$has_units = x %>% 
#     filter(!is.na(weight_units)) %>%
#     mutate(weight_units = as.character(weight_units))
#   x = x %>% filter(!tempID %in% out_units$has_units$tempID)
#   #NA in weight_units and no units in weight_kg field
#   out_units$missing_units = x %>% filter(!grepl("kg|kilogram|kilo|g|gram|mg|lb|pound", weight_kg))
#   if(nrow(out_units$missing_units)){
#     out_units$missing_units$weight_units = "missing_units"
#   } else {
#     out_units$missing_units$weight_units = as.character(out_units$missing_units$weight_units)
#   }
#   x = x %>% filter(!tempID %in% out_units$missing_units$tempID)
#   out_units$weight_units = x %>% filter(grepl("kg|kilogram|kilo|g|gram|mg|lb|pound", weight_kg))
#   x = x %>% filter(!tempID %in% out_units$weight_units$tempID)
#   
#   if(nrow(out_units$weight_units)){
#     out_units$weight_units$weight_units = lapply(seq_len(nrow(out_units$weight_units)), function(i){
#       ifelse(grepl("kg|kilogram|kilo", out_units$weight_units$weight[i]), "kg",
#              ifelse(grepl("mg|milligram", out_units$weight_units$weight[i]), "mg",
#                     ifelse(grepl("g|gram", out_units$weight_units$weight[i]), "g",
#                            ifelse(grepl("lb|pound", out_units$weight_units$weight[i]), "lb", "missing_units")))
#       )
#     }) %>% unlist()
#     #Remove weight units from weight
#     rm_list = c("kilogram", "kilograms", "kg", 
#                 "milligram", "milligrams", "mg", 
#                 "gram", "grams", "g",
#                 "pound", "pounds", "lb", "lbs")
#     out_units$weight_units = out_units$weight_units %>%
#       mutate(weight_kg = gsub(paste0(rm_list, collapse="|"), "", weight_kg))
#   }
#   
#   #Remove empty list elements
#   out_units = out_units[sapply(out_units, nrow) > 0]
#   return(out_units %>% bind_rows())
# }