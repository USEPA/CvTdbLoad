# Script of various utility functions to screens/prep CvT data for loading
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)

#'@description A function to load and pull all sheets from files in the specified directory.
#'It corrects for missing required column names from a template file by filling with NA values.
#'@param fileName The file name or path for the file of interest
#'@param template_path The file path for the extraction template. If not supplied, hard coded columns will be used.
#'@import readxl magrittr
#'@return A dataframe of the combined sheets
load_sheet_group <- function(fileName="", template_path=""){
  
  template = tryCatch({
    template_sheets = readxl::excel_sheets(template_path)
    lapply(template_sheets, function(s){
      names(readxl::read_excel(template_path, sheet=s))
    }) %T>% { names(.) <- template_sheets }   
  },
  error=function(cond){ message("...Error: ", cond); return(NULL) }
  )
  
  if(is.null(template)){
    message("...passed template file path doesn't exist...using default template...")
    template = list("Documents" = c("pmid", "other_study_identifier", "doi", "first_author", 
                                    "year", "title","url", "curator_comment"),
                    "Studies" = c("id", "test_substance_name", "test_substance_name_secondary", 
                                  "test_substance_casrn", "dose_level", "dose_level_units", 
                                  "administration_route", "dose_duration", "dose_frequency",     
                                  "dose_vehicle", "dose_volume", "fasting_period", "author_comment", 
                                  "curator_comment", "dermal_dose_vehicle", "dermal_dose_vehicle_pH", 
                                  "dermal_applied_area", "dermal_applied_area_units",
                                  "aerosol_particle_diameter_mean", "aerosol_particle_diameter_gsd", 
                                  "aerosol_particle_diameter_units", "aerosol_particle_density",
                                  "aerosol_particle_density_units"),
                    "Subjects" = c("id", "species", "subtype", "sex", "age", "age_units", "age_category", 
                                   "height", "height_units", "weight", "weight_units", "curator_comment"),
                    "Series" = c("id", "analyte_name", "analyte_name_secondary", 
                                 "analyte_casrn", "figure_name", "figure_type", 
                                 "figure_series_identifier", "x_min", "x_max", "y_min", "y_max", 
                                 "time_units", "conc_units", "log_conc_units", "loq", "loq_units", 
                                 "lod", "lod_units", "analytical_method_detail", 
                                 "radiolabeled", "fk_study_id", "fk_subject_id", "n_subjects_in_series", 
                                 "conc_medium", "curator_comment"),
                    "Conc_Time_Values" = c("fk_series_id", "time", "conc", "conc_sd", "conc_lower_bound", 
                                           "conc_upper_bound", "curator_comment"))
  }
  
  tryCatch({
    sheetNames = readxl::excel_sheets(fileName)
    sheetNames = sheetNames[sheetNames %in% names(template)]
    lapply(sheetNames, function(x){
      if(!x %in% names(template)){
        return(NULL)
      }
      tmp = readxl::read_xlsx(fileName, sheet = x, col_types = "text",
                              .name_repair = ~ ifelse(nzchar(.x), .x, paste0("missing_col_", LETTERS[seq_along(.x)])))
      #Get list of columns corresponding to a sheet from the template
      colList = switch(x, 
                       "Documents" = template$Documents,
                       "Studies" = template$Studies,
                       "Subjects" = template$Subjects,
                       "Series" = template$Series,
                       "Conc_Time_Values" = template$Conc_Time_Values)
      #Fill missing columns with NA
      tmp[colList[!colList %in% names(tmp)]] <- NA
      tmp =  select(tmp, all_of(colList))
    }) %T>% {
      names(.) <- sheetNames
    }
    
  },
  error=function(cond){ message("Error message: ", cond); return(NULL) }
  ) %>% return()
}

#'@description A function to load and pull all sheets from files already loaded to CvTdb.
#'It corrects for missing required column names from a template file by filling with NA values.
#'@param docID The CvT document ID for the file of interest
#'@param template_path The file path for the extraction template. If not supplied, hard coded columns will be used.
#'@import readxl magrittr
#'@return A dataframe of the combined sheets
load_database_sheet_group <- function(docID, template_path){
  sheet_list = c("documents", "studies", "subjects", "series", "conc_time_values")
  template_map = readxl::read_xlsx("input/template_map.xlsx")
  
  template = tryCatch({
    template_sheets = readxl::excel_sheets(template_path)
    lapply(template_sheets, function(s){
      tmp = paste0(s, ".", names(readxl::read_excel(template_path, sheet=s))) %>% 
        tolower()
      if(s == "Documents"){
        return(c(tmp, "documents.id"))
      } else if(s == "Studies"){
        return(c(tmp, "studies.fk_extraction_document_id"))
      } else if(s == "Series") {
        return(c(tmp, "time_units_original", "conc_units_original"))
      } else if(s == "Conc_Time_Values"){
        return(c(tmp, "time_original", "conc_original"))
      } else {
        return(tmp)
      }
    }) %T>% { names(.) <- template_sheets }   
  },
  error=function(cond){ message("...Error: ", cond); return(NULL) }
  )
  
  if(is.null(template)){ return(NA) } 
  #pull database data with template fields
  out = lapply(sheet_list, function(s){
    if(s == "documents"){
      tmp = query_cvt("SELECT * FROM documents WHERE extracted = 1") 
    } else {
      tmp = query_cvt(paste0("SELECT * FROM ", s))   
    }
    
    if(s == "conc_time_values"){
      tmp = dplyr::rename(tmp, conc_normalized = conc,
                          conc_lower_bound_normalized=conc_lower_bound,
                          conc_upper_bound_normalized=conc_upper_bound,
                          conc_sd_normalized = conc_sd)
    }
    tmp = tmp %T>% {
      #Have to map CvT database names back to the template (usually a _original stem)
      message("...Renaming mapped variables...", Sys.time())
      names(.)[names(.) %in% template_map$from[template_map$sheet == s]] <- left_join(data.frame(from=names(.)[names(.) %in% 
                                                                                                                 template_map$from[template_map$sheet == s]], 
                                                                                                 stringsAsFactors = F), 
                                                                                      template_map[template_map$sheet==s,], 
                                                                                      by = "from") %>% 
        select(to) %>% mutate(to = as.character(to)) %>% unlist()
      message("...Returning raw data...", Sys.time()) 
    } %>% rename_all(function(x){paste0(s,".", x)})
    
    #Get list of columns corresponding to a sheet from the template
    colList = switch(s, 
                     "documents" = template$Documents,
                     "studies" = template$Studies,
                     "subjects" = template$Subjects,
                     "series" = template$Series,
                     "conc_time_values" = template$Conc_Time_Values)
    #Fill missing columns with NA
    tmp[colList[!colList %in% names(tmp)]] <- NA
    message("Returning data for: ", s)
    return(select(tmp, all_of(colList)))
  }) %T>% { names(.) <- sheet_list }
  #Combine all data
  out = out$documents %>%
    left_join(out$studies, by=c("documents.id"="studies.fk_extraction_document_id"), keep=TRUE) %>%
    left_join(out$series, by=c("studies.id"="series.fk_study_id"), keep=TRUE) %>%
    left_join(out$subjects, by=c("series.fk_subject_id"="subjects.id"), keep=TRUE) %>%
    left_join(out$conc_time_values, by=c("series.id"="conc_time_values.fk_series_id"), keep=TRUE)
  #Split into subgroups
  out = lapply(unique(out$documents.id), function(d){
    doc = out %>% filter(out$documents.id == d)
    lapply(template_sheets, function(s){
      #Get list of columns corresponding to a sheet from the template
      colList = switch(s, 
                       "Documents" = template$Documents,
                       "Studies" = template$Studies,
                       "Subjects" = template$Subjects,
                       "Series" = template$Series,
                       "Conc_Time_Values" = template$Conc_Time_Values)
      #Fill missing columns with NA
      doc[colList[!colList %in% names(doc)]] <- NA
      doc =  select(doc, all_of(colList)) %>% 
        distinct() %T>%{ #Important T-operator
          colnames(.) = sub('.*\\.', '', colnames(.)) #Remove Prefixes
        }
    }) %T>% { names(.) <- template_sheets }
  }) %T>% { names(.) <- unique(out$documents.id ) }
    
  
  for(i in seq_len(length(names(out)))){
    # if(i < 24){#Quick skip for testing purposes
    #   next
    # }
    #
    
    f = paste0("load_CvT_doc_id_", names(out)[i])
    # if(!f %in% paste0("load_CvT_doc_id_", c(25, 56, 192, 138, 141))){
    #   next
    # }
    ######insert loop over fileList logic########
    message("Pushing file (", i, "/", length(names(out)),"): ", f, "...", Sys.time())
    #Create/clear log entry for filename
    log_CvT_doc_load(f, m=NULL, reset=TRUE)
    #Load Documents Sheet
    doc_sheet_list = out[[i]]
    
    doc_sheet_list$Subjects$species = normalize_species(x=doc_sheet_list$Subjects$species)
    
    #Call to the orchestration function for data normalization (with error logging)
    doc_sheet_list = normalize_CvT_data(df=doc_sheet_list, f=f)
    
    #If any issues were logged during normalization, don't push the doc
    if(log_check(basename(f))){
      message("...file has logged issues...skipping doc")
      next
    }
  }
}

log_CvT_doc_load <- function(f, m=NULL, reset=FALSE){
  if(file.exists("output\\template_normalization_log.xlsx")){
    log = readxl::read_xlsx("output\\template_normalization_log.xlsx")
    log$timestamp = as.character(log$timestamp)  
  } else {
    log = data.frame(filename=f, timestamp=as.character(Sys.time()))
  }
  
  #Add a new flag column if it doesn't exist
  if(!is.null(m)){
    if(!m %in% names(log)){
      log[[m]] <- 0
    }  
  }
  if(f %in% log$filename){
    if(reset){#Reset to 0 for entry
      log[log$filename == f, names(log)[!names(log) %in% c("filename", "timestamp")]] <- 0
    }
    if(!is.null(m)){
      #Set new flag
      log[log$filename == f, m] <- 1
      log[log$filename == f, "timestamp"] <- as.character(Sys.time())  
    }
  } else {
    tmp = setNames(data.frame(matrix(ncol = length(log), nrow = 1)), names(log))
    tmp[, names(tmp)[!names(tmp) %in% c("filename", "timestamp")]] <- 0
    tmp$filename = f
    if(!is.null(m)){
      tmp[m] = 1  
    }
    tmp$timestamp <- as.character(Sys.time())
    log = rbind(log, tmp)
  }
  writexl::write_xlsx(log, "output\\template_normalization_log.xlsx")
}

#'@description A helper function to check if a file has logged issues (changed to 
#'1 for select columns) and move it to appropriate subfolder.
reorganize_file_flags <- function(){
  log = readxl::read_xlsx("output\\template_normalization_log.xlsx")
  flag_map = readxl::read_xlsx("input\\dictionaries\\flag_map.xlsx")
  for(i in seq_len(nrow(log))){
    # if(i <= 7){#Quick skip/restart logic
    #   next
    # }
    f = log$filename[i]
    if(file.exists(paste0("output/normalized_templates/",gsub(".xlsx", "_normalized.xlsx", basename(f))))){
      for(flag in unique(flag_map$`Flag Type`)){
        # Get flag subfolder path
        f_path = switch(flag,
                      "Warning" = "output/normalized_templates/flagged/warning/",
                      "Hard Stop (Missing Required)" = "output/normalized_templates/flagged/hard_stop/missing_required/",
                      "Hard Stop (Need Split)" = "output/normalized_templates/flagged/hard_stop/need_split/",
                      "Hard Stop (Impossible Value)" = "output/normalized_templates/flagged/hard_stop/impossible_value/",
                      "Hard Stop (Conversion Failed)" = "output/normalized_templates/flagged/hard_stop/conversion_failed/",
                      "Hard Stop (Empty Sheet)" = "output/normalized_templates/flagged/hard_stop/empty_sheet/",
                      "Soft Stop (Conversion Needed)" = "output/normalized_templates/flagged/soft_stop/conversion_needed/",
                      "Soft Stop (Dictionary Update)" = "output/normalized_templates/flagged/soft_stop/dictionary_update/",
                      "Soft Stop (Clowder Doc Missing)" = "output/normalized_templates/flagged/soft_stop/clowder_missing/"
        )
        if(!is.null(f_path)){
          # Check if file was flagged with any corresponding flag for a category
          if(any(
            log[log$filename == f, 
                    names(log)[names(log) %in% flag_map$`Field Name`[flag_map$`Flag Type` == flag]]  
                     ]
            )
            ){
            message("Moving file to: ", f_path)
            file.rename(from=paste0("output/normalized_templates/",gsub(".xlsx", "_normalized.xlsx", basename(f))),
                        to=paste0(f_path, gsub(".xlsx", "_normalized.xlsx", basename(f)))
            )
            break #If moved, skip any further checks
          }
        }
      }
    }
  }
}

#'@description A helper function to match a new study entry to existing studies entries (if available).
#'In development, waiting to figure out bset way to match given string entries.
match_subject_fk <- function(df){
  
  subjects = query_cvt("SELECT * FROM subjects") %>%
    select(-source_system, -rec_update_dt, -rec_create_dt)
}

#'@description Helper function TBD.
match_chemical_fk <- function(df){
  
  chems = query_cvt("SELECT DISTINCT dsstox_substance_id FROM cvt.chemicals")
  
}

#'@description A helper function to convert input values to desired units.
#'@param x Input dataframe to convert
#'@param num Name of column with values to convert
#'@param units Name of column with units to convert from
#'@param desired Desired units to convert the input value into
#'@param MW Molecular weight of the input chemical (if applicable)
#'@param overwrite_units Boolean to overwrite the 'units' with desired units.
convert_units <- function(x, num, units, desired, MW=NA, overwrite_units=FALSE){
  #Map of input units to desired output units equation
  conv = list(day = list(hr="*24", day="/1", week="/7", month="/30", year="/365"),
              week = list(hr="*24*7", day="*7", week="/1", month="/4", year="/52"),
              month = list(day="*30", week="*4", month="/1", year="/52"),
              year = list(day="*365", week="*52", month="*12", year="/1"),
              kg = list(kg="/1"), #Only care to convert to kg for all weights
              g = list(mg="*1000", kg="/1000"),
              ug = list(mg="/1000"),
              `µg` = list(mg="/1000"),
              mg = list(mg="/1", kg="/1000000"),
              lb = list(kg="/2.2"),
              mm = list(cm="/10"), #Only care to convert to cm for all heights
              cm = list(cm="/1"),
              m = list(cm="*100"),
              `in`=list(cm="*2.54"),
              ft=list(cm="*12*2.54"),
              s=list(hr="/60/60"), #Only care to convert to hr f time
              min=list(hr="/60"),
              hr=list(hr="/1"),
              `mg/kg`=list(`mg/kg`="/1", `ug/ml`=paste0("*", MW)), #1 mg/kg*MW kg/L*1L/1000mL*1000ug/mg=ug/mL --> using httk density value for MW variable (refactor name)
              `ug/kg`=list(`mg/kg`="/1000", `ug/ml`=paste0("*", MW, "/1000")),
              `µg/kg` = list(`ug/ml`=paste0("*", MW, "/1000")), 
              `g/kg`=list(`mg/kg`="*1000"),
              `ug/250 g`=list(`mg/kg`="*4/1000"),
              `ug/ml`=list(`ug/ml`="/1"),
              `ug/l`=list(`ug/ml`="/1000"),
              `ng/ml`=list(`ug/ml`="/1000"),
              `ng/l`=list(`ug/ml`="/1000000"),
              `mg/ml`=list(`ug/ml`="*1000"),
              `mg/l`=list(`ug/ml`="/1"),
              ppm=list(`ug/ml`="/1"), #1 ppm = 1 ug/mL
              ppbv = list(`ug/ml`="/1000"), #1 ppb = 0.001 ug/mL,
              ppb = list(`ug/ml`="/1000"), #1 ppb = 0.001 ug/mL,
              `nmol/l` = list(`ug/ml`=paste0("*",MW,"/1000000")), #1 nmol/L*(1mol/1000000000nmol)*(MW g/1mol)*(1000000ug/1g)*(1L/1000mL)=1*MW/1000000
              `nmol/ml` = list(`ug/ml`=paste0("*",MW,"/1000")),
              `nmoles/ml` = list(`ug/ml`=paste0("*",MW,"/1000")),
              `umol/l` = list(`ug/ml`=paste0("*",MW,"/1000")), #1000 less than nmol/l conversion 
              `pmol/ml` = list(`ug/ml`=paste0("*",MW,"/1000000")), #1 pmol/ml*(1mol/1000000000000pmol)*(MW g/1mol)*(1000000ug/1g)=1*MW/1000000
              `ug/g` = list(`ug/ml`=paste0("*", MW)), #1 ug/g*1000g/kg*MW kg/L*1L/1000mL=ug/mL --> using httk density value for MW variable (refactor name)
              `umol/kg` = list(`mg/kg`= paste0("*", MW, "/1000")) # MW is g/mol, which is the same as mg/mmol or ug/umol
              )
  #Convert units based on input string equation
  if(is.null(conv[[x[[units]]]][[desired]])){
    #No matching desired output
    x[[num]] = NA
  } else {
    #Get the conversion equation (e.g. 20 days to weeks is '20/7')
    equ = paste0(x[[num]], conv[[x[[units]]]][[desired]])
    x[[num]] = parse(text=equ) %>% #parse the string
      eval() %>% #evaluate the string equation
      round(., 5) #round to 5 decimal places
    if(overwrite_units){
      x[[units]] = desired #Set to converted units  
    }
  }
  return(x) 
}

#'@description Function to return a dataframe of files ready to push due to all
#'flags being '0'.
get_cvt_push_ready <- function(){
  readxl::read_xlsx("output\\template_normalization_log.xlsx") %>% 
    filter(across(.cols=names(.)[!names(.) %in% c("filename", "timestamp")], 
                  .fns = ~. == 0)) %>%
    return()
}

#Generic function to extract units from input columns
extract_units <- function(x, units_col, conv_col, unit_type){
  #units_col = "weight_units"
  #conv_col = "weight_kg"
  #unit_type = "weight"
  conv_list = convert_units_grepl(unit_type)
  out_units = list()
  
  # #Has units field
  # out_units$has_units = x %>%
  #   filter(!is.na(!!as.symbol(units_col)))
  # x = x %>% filter(!tempID %in% out_units$tempID)
  
  #NA in weight_units and no units in weight_kg field
  out_units$missing_units = x %>% 
    filter(!grepl(paste0(conv_list %>% unlist() %>% unname(), collapse="|"), !!as.symbol(units_col)))
  x = x %>% filter(!tempID %in% out_units$missing_units$tempID)
  out_units$conv_ready = x %>% 
    filter(grepl(paste0(conv_list %>% unlist() %>% unname(), collapse="|"), !!as.symbol(units_col)))
  x = x %>% filter(!tempID %in% out_units$conv_ready$tempID)
  
  if(nrow(out_units$missing_units)){
    #Attempt to extract from conv_col
    #out_units$missing_units[[units_col]] = "missing_units"
    out_units$missing_units[[units_col]] = lapply(seq_len(nrow(out_units$missing_units)), function(i){
      for(conv in names(conv_list)[!names(conv_list) %in% c("rm_list")]){
        if(grepl(conv_list[[conv]], out_units$missing_units[[conv_col]][i])){
          return(conv)
        }
      }
      return("missing_units")
    }) %>% unlist()
    #Remove units from converted column
    out_units$missing_units = out_units$missing_units %>%
      #https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
      mutate(across(.cols=all_of(conv_col), .fns= ~gsub(paste0(conv_list$rm_list, ".*", collapse="|"), "", .) %>%
                      gsub("old", "", .) %>%
                      #https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r/24173271
                      gsub("\\s*\\([^\\)]+\\)","", .) %>%
                      gsub(">|<|at least", "", .)
                    ))
  }
  
  #Remove empty list elements
  out_units = out_units[sapply(out_units, nrow) > 0]
  return(out_units %>% bind_rows())
}

#Function to get various grepl statements for unit extraction/convertion
convert_units_grepl <- function(unit_type){
  switch(unit_type, 
         "weight" = list(kg = "kg|kilogram|kilo",
                         mg = "mg|milligram",
                         g = "g|gram",
                         lb = "lb|pound",
                         rm_list = c("kilogram", "kilograms", "kg", 
                                     "milligram", "milligrams", "mg", 
                                     "gram", "grams", "g",
                                     "pound", "pounds", "lb", "lbs")
                         ),
         "height" = list(cm = "cm|centimeter",
                         mm = "millimeter|mm",
                         m = "m|meter",
                         `in`="in|inch",
                         ft="ft|foot|feet",
                         rm_list = c("cm", "centimeter", "centimeters", 
                                     "mm", "millimeter", "millimeters", 
                                     "m", "meter", "meters",
                                     "in", "inch", "inches",
                                     "ft", "foot", "feet")
                         ),
         "age" = list(week = "week|weeks|wk|wks",
                      year = "years old|year|years|yr|yrs",
                      day = "day|days|GD|gestation",
                      month = "month|months",
                      rm_list = c("week", "weeks","wk","wks",
                                  "month",
                                  "years","year","-year","yr","yrs",
                                  "day","days","GD","gestational day","gestational days")),
         "dose_duration" = list(day = "day|days|GD",
                                week = "week|weeks|wk|wks",
                                month = "month|months",
                                min = "min|mins",
                                hr = "hour|hours|hr|hrs|h",
                                s = "sec|s|second|seconds",
                                rm_list= c("within", "day",
                                           "week", "wk", "wks",
                                           "month",
                                           "min", "mins",
                                           "hour", "hr", "hrs", "h",
                                           "sec", "s"))
         ) %>%
    return()
}

#'@description A helper function to cache the normalized templates
save_normalized_template <- function(df=doc_sheet_list, f=f){
  #"L:\Lab\HEM\T_Wall_Projects_FY20\CvT Database\output\normalized_templates"  
  fn = basename(f)
  fn_ext = tools::file_ext(fn)
  fn = gsub(paste0(".", fn_ext), "", fn)
  writexl::write_xlsx(x=doc_sheet_list, path=paste0("output/normalized_templates/", basename(fn), "_normalized.", fn_ext))
}

#'@description A helper function to convert input dataframe column list to NA
convert_cols_to_NA <- function(df, col_list){
  df[col_list] = NA
  return(df)
}


#'@description A helper function to orchestrate normalization of CvT Data
normalize_CvT_data <- function(df, f){
  df$Subjects = normalize_weight(raw=df$Subjects, f=f)
  df$Subjects = normalize_height(raw=df$Subjects, f=f)
  df$Subjects = normalize_age(raw=df$Subjects, f=f)
  #"SELECT c.id, s.id, c.fk_series_id, c.time_original, s.time_units_original, c.time_hr FROM series s LEFT JOIN conc_time_values c on s.id = c.fk_series_id"
  #Normalize time requires Series and Conc_Time_Values
  tmp = normalize_time(raw = df$Series %>% 
                         left_join(df$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
                         select(id, time_original=time, time_units_original=time_units), 
                       f = f)
  df$Conc_Time_Values = df$Conc_Time_Values %>% 
    mutate(tempID = seq_len(nrow(df$Conc_Time_Values))) %>%
    left_join(tmp, by=c("tempID", "fk_series_id"="id")) %>% 
    select(-time, -time_units_original, -tempID)
  #Check radiolabel for evidence of radiolabeled chemicals or analytes
  check_radiolabel(raw=df$Series %>%
                     select(analyte_name, analyte_name_secondary, fk_study_id, radiolabeled) %>%
                     left_join(df$Studies %>% select(id, test_substance_name), by=c("fk_study_id"="id")),
                   f=f) #Combine Study and Series chemical information
  #Convert to boolean (assumes NA = 0, else is 1)
  df$Series = normalize_boolean(x=df$Series, col=c("radiolabeled", "log_conc_units"))
  #Convert tp character
  if(!is.character(df$Series$n_subjects_in_series)){
    df$Series$n_subjects_in_series = as.character(df$Series$n_subjects_in_series)  
  }
  #Harmonize conc_medium
  df$Series=normalize_conc_medium(raw=df$Series, f=f)
  #Normalize Dose
  tmp = normalize_dose(raw = df$Series %>%
                         left_join(df$Studies, by=c("fk_study_id"="id")) %>%
                         left_join(df$Subjects, by=c("fk_subject_id"="id")) %>%
                         select(fk_study_id, species, subtype, weight_kg, height_cm,
                                test_substance_name, dose_level, dose_level_units, dose_volume, administration_route_normalized),
                       f=f) %>%
    select(fk_study_id, dose_level_normalized) %>% distinct()
  df$Studies = df$Studies %>%
    left_join(tmp, by=c("id"="fk_study_id"))
  #Normalize Conc Units
  tmp = normalize_conc(raw=df$Series %>%
                         left_join(df$Subjects %>%
                                     select(id, species), by=c("fk_study_id"="id")) %>%
                         left_join(df$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
                         select(id, species, conc_medium, analyte_name, analyte_name_secondary, analyte_casrn,
                                conc_original=conc, conc_units_original=conc_units,
                                conc_sd_original=conc_sd, conc_lower_bound_original=conc_lower_bound,
                                conc_upper_bound_original=conc_upper_bound), 
                             f=f) %>%
    select(-conc_medium, -analyte_name, -analyte_name_secondary, -analyte_casrn, -conc_units_original)
  df$Conc_Time_Values = df$Conc_Time_Values %>%
    mutate(tempID = seq_len(nrow(df$Conc_Time_Values))) %>%
    select(-conc, -conc_sd, -conc_lower_bound, -conc_upper_bound) %>%
    left_join(tmp, by=c("tempID")) %>%
    select(-tempID)
  
  df$Studies[c("dose_duration", "dose_duration_units")] = normalize_dose_duration(df$Studies %>% select(dose_duration, dose_duration_units))
  
  return(df)
}
