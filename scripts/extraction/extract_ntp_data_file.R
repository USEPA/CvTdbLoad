# Script to automate extraction for NTP style CvT data documents
# Created by: Jonathan Taylor Wall
# Created Date: 2023-03-8
#Load packages
# require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl); library(httk)
# R version 4.1.0 (2021-05-18)
# httk_2.1.0; readxl_1.3.1; tidyr_1.2.0; magrittr_2.0.2; dplyr_1.0.8; DBI_1.1.2
# purrr_0.3.4; assertthat_0.2.1
#Load R Scripts

#' extract_ntp_data_file
#' @description Function to semi-automate extraction of NTP data to CvT template
#' @param filepath File path to the NTP file to extract to CvT template
#' @param template_path File path to latest template
#' @param template_map File path to a field name map from NTP to CvT template names
extract_ntp_data_file <- function(filepath, 
                                  template_path = "input/CvT_data_template_articles.xlsx", 
                                  template_map = "input/ntp_template_map.xlsx"){
  if(!is.null(filepath) & is.na(filepath)) return(NULL)
  if(!file.exists(filepath)) return(NULL)
  
  # Get sheet list (species - aministration_route)
  s_list = readxl::excel_sheets(filepath)
  
  # Pull data from all sheets
  in_dat = lapply(s_list, function(s){
    tmp = readxl::read_xlsx(filepath, sheet=s)
    # if(s != "Intro"){
    #   # Filter fields when not Intro sheet
    #   tmp = tmp %>%
    #     filter(!is.na(Species), !is.na(Analyte), Analyte != "NA")
    # }
     return(tmp)
  }) %T>% {
    names(.) <- s_list
  }
  
  # Get intro data separated and transformed into usable format
  intro_dat = in_dat %>% 
    purrr::pluck("Intro") %>%
    rbind(paste("Title:", names(.))) %T>% {
      names(.) <- "intro_metadata"
    } %>%
    filter(!is.na(intro_metadata)) %>%
    tidyr::separate(intro_metadata, into=c("field_name", "value"), sep=": ")
  
  # Remove intro data sheet, then combine
  in_dat = in_dat %>%
    purrr::list_modify("Intro" = NULL) %>%
    dplyr::bind_rows() # %>%
    # Filter out any "NA" analyte entries
    # filter(!is.na(Analyte))
  
  # Pull templates to map fields
  template = get_cvt_template(template_path = template_path)
  map = readxl::read_xlsx(template_map)
  
  # Loop through the template and populate the fields
  # Use field map to select and populate
  # Map field names to template
  out = lapply(names(template)[!names(template) %in% c("Conc_Time_Values")], function(s){
    message("Working on sheet: ", s)
    # Update map to include concentration columns dynamically
    if(s  == "Series"){
      map = rbind(map, 
                  data.frame(from=names(in_dat)[grepl("Concentration", names(in_dat))]) %>%
                    mutate(to=from,
                           sheet=tolower(s)))  
    } else if (s == "Subjects"){
      map = rbind(map, 
                  data.frame(from=names(in_dat)[grepl("Weight", names(in_dat))]) %>%
                    mutate(to=from,
                           sheet=tolower(s)))  
    }
    
    # Select and rename/map columns of interest
    tmp = in_dat %>%
      # Select columns of interest
      select(any_of(map$from[map$sheet == tolower(s)])) %T>% {
      # Have to map CvT database names back to the template (usually a _original stem)
      message("...Renaming mapped variables...", Sys.time())
      names(.)[names(.) %in% map$from[map$sheet == tolower(s)]] <- left_join(data.frame(from=names(.)[names(.) %in% 
                                                                                                        map$from[map$sheet == tolower(s)]], 
                                                                                        stringsAsFactors = F), 
                                                                             map[map$sheet==tolower(s),], 
                                                                             by = "from") %>% 
        select(to) %>% 
        mutate(to = as.character(to)) %>% 
        unlist()
      message("...Returning converted data...", Sys.time())
      } %>%
      distinct()
    
    # Sheet specific transformations
    if(s == "Documents"){
      tmp = tmp %>%
        mutate(title = intro_dat$value[intro_dat$field_name == "Title"],
               year = intro_dat$value[intro_dat$field_name == "Start Date"],
               other_study_identifier = intro_dat$value[intro_dat$field_name == "NTP Study Number"],
               extracted = 1,
               document_type = 1)
    } else if(s == "Studies"){
      # Some NTP studies do not have a "Test Article" field which this maps for
      if(!"test_substance_name" %in% names(tmp)){
        tmp$test_substance_name = intro_dat$value[intro_dat$field_name == "Compound Name"]
        tmp$test_substance_casrn = intro_dat$value[intro_dat$field_name == "CASRN"]
      }
      
      # Run through cases to adjust/rename or split out
      fix_cols = names(tmp)[!names(tmp) %in% names(template[[s]])]
      
      for(f_col in fix_cols){
        # Handle dose volume/units splitting
        if(grepl("volume", f_col, ignore.case = TRUE)){
          tmp = tmp %>%
            dplyr::rename(dose_volume = f_col) %>%
            dplyr::mutate(dose_volume_units = f_col %>%
                            # Extract inside parentheses
                            stringr::str_extract(., "(?<=\\().*(?=\\))")) %>%
            arrange(test_substance_name, dose_level, administration_route)
        } else {
          # Catch any future unhandled
          stop("Unhandled studies field: ", f_col)
        }
      }
    } else if(s == "Subjects"){
      # Qualify the Animal ID in comments to assist with QC
      tmp$curator_comment = paste0("Animal ID: ", tmp$curator_comment, "_", tmp$sex)
      
      # TODO Handle multiple weight columns...just like concentration
      # Split columns based on conc_medium
      tmp = tmp %>%
        # Splitting out columns with units in the name
        tidyr::pivot_longer(names_to = "weight_units", values_to = "weight", cols=contains("Weight")) %>%
        mutate(weight_units = weight_units %>%
                 # Extract inside parentheses
                 stringr::str_extract(., "(?<=\\().*(?=\\))"),
               weight = suppressWarnings(as.numeric(weight))) %>%
        # Grouped filtering to find weight values (remove duplicates)
        group_by(curator_comment) %>%
        filter(weight == max(weight, na.rm=TRUE))
      
      # Run through cases to adjust/rename or split out
      fix_cols = names(tmp)[!names(tmp) %in% names(template[[s]])]
      
      for(f_col in fix_cols){
        # Catch any future unhandled
        stop("Unhandled subjects field: ", f_col)
      }
    } else if(s == "Series") {
      # Some NTP studies do not have a "Test Article" or CASRN field which this maps for
      if(!"test_substance_name" %in% names(tmp)){
        tmp$test_substance_name = intro_dat$value[intro_dat$field_name == "Compound Name"]
      }
      
      # Figure name as Animal ID
      tmp$figure_name = paste0("Animal ID: ", tmp$figure_name)
      
      # Split columns based on conc_medium
      tmp = tmp %>%
        # Splitting out columns with units in the name
        tidyr::pivot_longer(names_to = "conc_medium", values_to = "conc", cols=contains("Concentration (")) %>%
        tidyr::separate(col=conc_medium, into=c("conc_medium", "conc_units"), sep="\\(") %>%
        # Clean up and set default fields
        dplyr::mutate(conc_medium = gsub("Concentration", "", conc_medium),
                      conc_units = gsub("\\)", "", conc_units),
                      across(c("conc_medium", "conc_units"), ~stringr::str_squish(.)),
                      figure_type = "Table",
                      log_conc_units = 0,
                      conc_cumulative = 0,
                      n_subjects_in_series = 1,
                      tmp_conc_id = 1:n())
      
      # Fix concentration specification fields to match conc_medium entries
      # Used on conc_time_values sheet later as comments
      tmp2 = tmp %>% 
        select(tmp_conc_id, contains("Concentration Specification")) %>%
        tidyr::pivot_longer(names_to = "conc_medium", values_to = "conc_curator_comment", cols=contains("Concentration Specification")) %>%
        dplyr::mutate(conc_medium = gsub("Concentration Specification", "", conc_medium),
                      across(c("conc_medium", "conc_curator_comment"), ~stringr::str_squish(.))) %>%
        distinct() %>%
        rowwise() %>%
        # Clean up NA cases
        dplyr::mutate(conc_curator_comment = ifelse((is.na(conc_curator_comment) | conc_curator_comment == "NA"), 
                                                    "", 
                                                    conc_curator_comment)) %>%
        ungroup()
      
      # Handle case where there's only 1 conc_medium specification, often called "Tissue Concentration Specification"
      if(length(unique(tmp2$conc_medium)) == 1){
        tmp2$conc_medium = unique(tmp$conc_medium)
      }
      
      # Re-join as comment field
      tmp = tmp %>%
        left_join(tmp2, 
                  by = c("tmp_conc_id", "conc_medium")) %>%
        select(-tmp_conc_id)
    }
    
    # Return distinct with sequential ID field
    tmp %>% 
      distinct() %>%
      # Add id sequence
      dplyr::mutate(id = 1:n()) %>%
      return()
  }) %T>% {
    names(.) <- names(template)[!names(template) %in% c("Conc_Time_Values")]
  }
  
  # Create conc_time_values data from splitting Series sheet
  out$Conc_Time_Values = out$Series %>%
    dplyr::select(fk_series_id = id, time, conc, figure_name, conc_curator_comment) %>%
    # Add id field
    dplyr::mutate(id = 1:n()) %>%
    # Format Animal ID and Concentration Specification columns to comment
    tidyr::unite(figure_name, conc_curator_comment, col="curator_comment", sep = "; ") %>%
    mutate(curator_comment = gsub("; $", "", curator_comment))

  # Handle foreign key linkages using matching columns
  out$Series = out$Series %>%
    left_join(out$Studies %>%
                select(fk_study_id=id, test_substance_name, dose_level, administration_route),
              by=c("test_substance_name", "dose_level", "administration_route"))
  
  out$Series = out$Series %>%
    left_join(out$Subjects %>%
                select(fk_subject_id = id, figure_name = curator_comment),
              by="figure_name")
  
  # Finish processing template
  out = lapply(names(template), function(s){
    tmp = out[[s]]
    # Fill missing template fields (happens when template is updated compared to older uploaded version)
    tmp[names(template[[s]])[!names(template[[s]]) %in% names(tmp)]] <- NA
    # Select and reorder template columns
    tmp %>%
      .[names(template[[s]])] %>%
      return()
  }) %T>% {
    names(.) <- names(template)
  }
  
  # Return processed template
  return(out)
}

#' get_cvt_template
#' Pull the CvT template in a list of empty dataframes
get_cvt_template <- function(template_path){
  s_list = readxl::excel_sheets(template_path)
  lapply(s_list, function(s){
    readxl::read_xlsx(template_path, sheet=s)
  }) %T>% { names(.) <- s_list } %>%
    return()
}