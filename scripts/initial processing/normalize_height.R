#'@description A helper function to normalize subject height.
#'@param raw A dataframe of height information to normalize
#'@param f The file name of the template being processed. Used for error logging.
#'
#'@return Normalized version of the input `raw` parameter.
normalize_height <- function(raw, f){
  message("...normalizing height...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Subjects %>% select(species, subtype, height, height_units) %>% distinct() %>%
  #     mutate(doc = f)
  # }) %>%
  #   bind_rows()
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_height dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = prep_normalization(x=raw, newcols=c("height_cm"))#, "height_estimated"))
  #Set to convert column to maintain original
  out$raw$height_cm = out$raw$height
  #Extract units
  #out$raw = extract_height_units(x=out$raw)
  out$raw = extract_units(x=out$raw, units_col="height_units", 
                          conv_col="height_cm", unit_type="height")
  #Extrapolate heights - no longer doing this
  #out = norm_extrapolate(x=out, f=f, extrap_type="height")
  #Missing height
  out = check_missing(x=out, miss_col = "height", f=f, flag=FALSE)
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="height_units")
  out$missing_units$height_units = NA #Replacing missing units to NA after flagging
  #List of heights
  out = check_subject_list(x=out, f=f, col="height_cm")
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="height_cm", estimated=c())#"height_estimated")
  #Height range
  out = check_unit_range(x=out, f=f, col="height_cm", estimated=c())#"height_estimated")
  #Ready for conversion
  out$conversion = out$raw %>% 
    mutate(height_cm = suppressWarnings(as.numeric(height_cm))) %>%
    filter(!is.na(height_cm)) #%>%
    #mutate(height_estimated = 0)
  out$raw = out$raw %>% filter(!tempID %in% out$conversion$tempID)
  
  if(nrow(out$raw)){
    message("...Unhandled cases for height: ", paste0(out$raw$height_cm %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_height_normalize_case")
  }
  #out$unhandled_cases = out$raw
  #Convert m, cm, mm, in, ft, etc.
  out$convert_ready = bind_rows(out$conversion, out$ci, out$unit_range)
  out$conversion = NULL; out$ci = NULL; out$unit_range = NULL
  for(i in seq_len(nrow(out$convert_ready))){
    out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                          num="height_cm", 
                                          units="height_units", desired="cm", 
                                          overwrite_units = FALSE)
  }
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("convert_ready")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("height_cm")) %>%
        return()
    }
  })
  #Convert height_cm to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% mutate(height_cm = suppressWarnings(as.numeric(height_cm)))
  })
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  return(out %>% bind_rows() %>% arrange(tempID))
}

#Deprecated function...
# extract_height_units <- function(x){
#   out_units = list()
#   #Has height_units field
#   out_units$has_units = x %>% 
#     filter(!is.na(height_units)) %>%
#     mutate(height_units = as.character(height_units))
#   x = x %>% filter(!tempID %in% out_units$has_units$tempID)
#   #NA height_units and no units in height_cm
#   out_units$missing_units = x %>% filter(!grepl("cm|m|centimeter|meter|millimeter|mm|in|ft|inch|foot|feet", height_cm))
#   if(nrow(out_units$missing_units)){
#     out_units$missing_units$height_units = "missing_units"  
#   } else {
#     out_units$missing_units$height_units = as.character(out_units$missing_units$height_units)
#   }
#   x = x %>% filter(!tempID %in% out_units$missing_units$tempID)
#   out_units$height_units = x %>% filter(grepl("cm|m|centimeter|meter|millimeter|mm|in|ft|inch|foot|feet", height_cm))
#   x = x %>% filter(!tempID %in% out_units$height_units$tempID)
#   
#   if(nrow(out_units$height_units)){
#     out_units$height_units$height_units = lapply(seq_len(nrow(out_units$height_units)), function(i){
#       ifelse(grepl("cm|centimeter", out_units$height_units$height[i]), "cm",
#              ifelse(grepl("millimeter|mm", out_units$height_units$height[i]), "mm",
#                     ifelse(grepl("m|meter", out_units$height_units$height[i]), "m",
#                            ifelse(grepl("in|inch", out_units$height_units$height[i]), "in", 
#                                   ifelse(grepl("ft|foot|feet", out_units$height_units$height[i]), "ft", "missing_units")
#                            )
#                     )
#              )
#       )
#     }) %>% unlist()
#     #Remove height units from height
#     rm_list = c("cm", "centimeter", "centimeters", 
#                 "mm", "millimeter", "millimeters", 
#                 "m", "meter", "meters",
#                 "in", "inch", "inches",
#                 "ft", "foot", "feet")
#     out_units$height_units = out_units$height_units %>%
#       mutate(height_cm = gsub(paste0(rm_list, collapse="|"), "", height_cm))
#   }
#   
#   #Remove empty list elements
#   out_units = out_units[sapply(out_units, nrow) > 0]
#   return(out_units %>% bind_rows())
# }