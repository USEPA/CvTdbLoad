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
  if(is.null(filepath) || is.na(filepath)) return(NULL)
  if(!file.exists(filepath)) return(NULL)
  
  # Get sheet list (species - aministration_route)
  s_list = readxl::excel_sheets(filepath)
  
  # Pull data from all sheets
  in_dat = lapply(s_list, function(s){
    tmp = readxl::read_xlsx(filepath, sheet=s)
    if(s != "Intro"){
      tmp = tmp %>%
        # Handle cases so bind_rows() can happen later
        mutate(across(everything(), ~suppressWarnings(as.character(.))))
    }
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
    tidyr::separate(intro_metadata, into=c("field_name", "value"), 
                    sep=": ", 
                    fill = "right")
  
  # # Remove intro data sheet, then combine
  # in_dat = in_dat %>%
  #   purrr::list_modify("Intro" = NULL) %>%
  #   dplyr::bind_rows() %>%
  #   # Filter out caption information
  #   filter(!grepl(paste0(c("BLOQ",
  #                          "All concentration data",
  #                          "NA = Not applicable",
  #                          "ND = Not detected.",
  #                          "Approximate value",
  #                           paste0(letters, "\\.")
  #   ), collapse="|"), `Animal ID`),
  #   !is.na(`Animal ID`))
  
  # Remove intro sheet
  in_dat = in_dat %>%
    purrr::list_modify("Intro" = NULL)

  in_dat = lapply(in_dat, function(s){
    s %>%
      # Filter out caption information
      filter(!grepl(paste0(c("BLOQ",
                             "All concentration data",
                             "NA = Not applicable",
                             "ND = Not detected.",
                             "Approximate value",
                             paste0(letters, "\\.")
      ), collapse="|"), `Animal ID`),
      !is.na(`Animal ID`)) %>%
      return()
  }) %T>% {
    names(.) <- names(in_dat)
  }
  
  # if(!nrow(in_dat)){
  #   stop("No data found after initial filter...")
  # }
    
  # Filter out any "NA" analyte entries
  # filter(!is.na(Analyte))
  
  # Pull templates to map fields
  template = get_cvt_template(template_path = template_path)
  map = readxl::read_xlsx(template_map)
  
  # TODO Loop through each in_dat sheet and process into template
  out <- lapply(names(in_dat), function(s){
    format_ntp_template(s_in_dat = in_dat[[s]], 
                        map=map, 
                        template=template,
                        sheetname=s
                        )
  }) %T>% {
    names(.) <- names(in_dat)
  }
  
  # TODO Combine sheets into single template (handle ID values)
  out_full <- lapply(names(template), function(s){
    out %>%
      purrr::map(s) %>%
      bind_rows() %>%
      return()
  }) %T>% {
    names(.) <- names(template)
  }
  
  # TODO Remap ID fields
  
  # TODO Make Documents and Studies Unique (remap ID)
  
  # Fill in analyte_casrn if analyte name is the same as the "Compound Name"
  out$Series$analyte_casrn[out$Series$analyte_name == toString(intro_dat$value[intro_dat$field_name == "Compound Name"])] = toString(intro_dat$value[intro_dat$field_name == "CASRN"])
  
  # Fill in "ND" for conc values
  out$Conc_Time_Values$conc[out$Conc_Time_Values$conc == "." & grepl("Not detected|ND|not detected", out$Conc_Time_Values$curator_comment)] = "ND"
  # Fill in "NQ" for below calibration curve range
  out$Conc_Time_Values$conc[out$Conc_Time_Values$conc == "." & grepl("calibration curve|limit of quantitation", out$Conc_Time_Values$curator_comment)] = "NQ"
  # Fill in "NA" for No data
  out$Conc_Time_Values$conc[out$Conc_Time_Values$conc == "." & grepl("No data", out$Conc_Time_Values$curator_comment)] = "NA"
  # Substitute BLOQ with NQ
  out$Conc_Time_Values$conc[out$Conc_Time_Values$conc == "BLOQ"] = "NQ"
  # Remove extraneous missing appended notes
  out$Conc_Time_Values$curator_comment = out$Conc_Time_Values$curator_comment %>%
    gsub("; NA", "", .)
  
  # Foreign key checks
  if(any(!out$Studies$fk_reference_document_id[!is.na(out$Studies$fk_reference_document_id)] %in% out$Documents$id)){
    stop("Nonexistent fk_reference_document_id present...")
  }
  if(any(is.na(out$Series$fk_subject_id)) | any(!out$Series$fk_subject_id %in% out$Subjects$id)){
    stop("Missing or Nonexistent fk_subject_id present...")
  }
  if(any(is.na(out$Series$fk_study_id)) | any(!out$Series$fk_study_id %in% out$Studies$id)){
    stop("Missing or Nonexistent fk_study_id present...")
  }
  if(any(is.na(out$Conc_Time_Values$fk_series_id)) | any(!out$Conc_Time_Values$fk_series_id %in% out$Series$id)){
    stop("Missing or Nonexistent fk_subject_id present...")
  }
  # Extraneous entries
  if(any(!out$Documents$id %in% c(out$Studies$fk_reference_document_id, 1))){
    warning("Extraneous fk_reference_document_id present in ", filepath)
  }
  if(any(!out$Subjects$id %in% out$Series$fk_subject_id)){
    warning("Extraneous fk_subject_id present in ", filepath)
  }
  if(any(!out$Studies$id %in% out$Series$fk_study_id)){
    warning("Extraneous fk_study_id present in ", filepath)
  }
  if(any(!out$Series$id %in% out$Conc_Time_Values$fk_series_id)){
    warning("Extraneous fk_subject_id present in ", filepath)
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

format_ntp_template <- function(s_in_dat, map, template, sheetname){
  # Check if any data to process
  if(!nrow(s_in_dat)){
    message("...No data found after initial filter...")
    return(NULL)
  }
  message("...Working on sheet: ", sheetname)
  # Loop through the template and populate the fields
  # Use field map to select and populate
  # Map field names to template
  out = lapply(names(template)[!names(template) %in% c("Conc_Time_Values")], function(s){
    message("......Working on sheet: ", s)
    # Update map to include concentration columns dynamically
    if(s  == "Series"){
      map = rbind(map, 
                  data.frame(from=names(s_in_dat)[grepl("Concentration|Time|Dose", names(s_in_dat))]) %>%
                    mutate(to=from,
                           sheet=tolower(s)))  
    } else if (s == "Subjects"){
      map = rbind(map, 
                  data.frame(from=names(s_in_dat)[grepl("Weight", names(s_in_dat))]) %>%
                    mutate(to=from,
                           sheet=tolower(s)))  
    } else if (s == "Studies"){
      map = rbind(map, 
                  data.frame(from=names(s_in_dat)[grepl("Dose", names(s_in_dat))]) %>%
                    mutate(to=from,
                           sheet=tolower(s)))  
    } else if(s == "Conc_Time_Values"){
      map = rbind(map, 
                  data.frame(from=names(s_in_dat)[grepl("Time", names(s_in_dat))]) %>%
                    mutate(to=from,
                           sheet=tolower(s)))  
    }
    
    # Create a named vector to handle renaming from the map
    s_map = map %>%
      filter(sheet == tolower(s))
    s_map = s_map %>%
      select(from) %>% 
      unlist() %T>% {
        names(.) <- s_map$to
      }
    
    # Select and rename/map columns of interest
    tmp = s_in_dat %>%
      dplyr::rename(any_of(s_map)) %>%
      select(any_of(names(s_map))) %>%
      distinct()
    
    # Sheet specific transformations
    if(s == "Documents"){
      tmp = tmp %>%
        mutate(title = toString(intro_dat$value[intro_dat$field_name == "Title"]),
               year = toString(intro_dat$value[intro_dat$field_name %in% c("Start Date", "Approval Date")]),
               other_study_identifier = toString(intro_dat$value[intro_dat$field_name == "NTP Study Number"]),
               extracted = 1,
               document_type = 1)
    } else if(s == "Studies"){
      # Some NTP studies do not have a "Test Article" field which this maps for
      if(!"test_substance_name" %in% names(tmp)){
        tmp$test_substance_name = toString(intro_dat$value[intro_dat$field_name == "Compound Name"])
        tmp$test_substance_casrn = toString(intro_dat$value[intro_dat$field_name == "CASRN"])
      }
      
      # Handle case of Dose Frequency/Unit columns
      if(all(c("Dose Frequency", "Dose Frequency Unit") %in% names(tmp))){
        tmp <- tmp %>%
          unite("dose_frequency_orig", `Dose Frequency`, `Dose Frequency Unit`, sep = " ")
        tmp$dose_frequency_orig[tmp$dose_frequency_orig == "NA NA"] <- NA
      }
      
      # Run through cases to adjust/rename or split out
      fix_cols = names(tmp)[!names(tmp) %in% names(template[[s]])]
      
      # Handle dose volume/units splitting
      f_col <- fix_cols[grepl("dose frequency|dose_frequency", fix_cols, ignore.case = TRUE)]
      if(length(f_col)) {
        tmp = tmp %>%
          tidyr::pivot_longer(cols=all_of(f_col), names_to = "dose_frequency_units", values_to = "dose_frequency") %>%
          dplyr::mutate(dose_frequency_units = stringr::str_extract(dose_frequency_units, "(?<=\\().*(?=\\))")) %>%
          tidyr::unite(col="dose_frequency", dose_frequency, dose_frequency_units, sep = " ") %>%
          dplyr::mutate(dose_frequency = gsub(" NA", "", dose_frequency) %>%
                          stringr::str_squish())
        tmp$dose_frequency[tmp$dose_frequency == "NA"] <- NA
      }
      # Remove handled cases
      fix_cols <- fix_cols[!fix_cols %in% f_col]
      
      # Handle dose volume/units splitting
      f_col <- fix_cols[grepl("volume", fix_cols, ignore.case = TRUE)]
      if(length(f_col)){
        tmp = tmp %>%
          dplyr::rename(dose_volume = all_of(f_col)) %>%
          dplyr::mutate(dose_volume_units = f_col %>%
                          # Extract inside parentheses
                          stringr::str_extract(., "(?<=\\().*(?=\\))"))
      }
      # Remove handled cases
      fix_cols <- fix_cols[!fix_cols %in% f_col]
      
      # Handle dose level/units splitting
      f_col <- fix_cols[grepl("dose", fix_cols, ignore.case = TRUE)]
      if(length(f_col)) {
        tmp = tmp %>%
          pivot_longer(cols=all_of(f_col), names_to = "dose_level_units", values_to = "dose_level") %>%
          mutate(dose_level_units = stringr::str_extract(dose_level_units, "(?<=\\().*(?=\\))"))
      }
      # Remove handled cases
      fix_cols <- fix_cols[!fix_cols %in% f_col]
      
      if(length(fix_cols)) stop("Unhandled studies field: ", toString(fix_cols))
      
      # Filter out those without a dose_level (happens due to spacer row in sheets before caption)
      tmp = tmp %>%
        dplyr::mutate(across(any_of(c("dose_level", "dose_frequency", "dose_volume")), ~suppressWarnings(as.numeric(.)))) %>%
        dplyr::filter(!is.na(dose_level)) %>%
        # Sort rows for ease of review
        arrange(test_substance_name, administration_route, dose_level, dose_level_units)
    } else if(s == "Subjects"){
      # Qualify the Animal ID in comments to assist with QC
      tmp$curator_comment = paste0("Animal ID: ", tmp$curator_comment, "_", tmp$sex)
      
      # Split columns based on weight
      tmp = tmp %>%
        # Splitting out columns with units in the name
        tidyr::pivot_longer(names_to = "weight_units", values_to = "weight", cols=contains("Weight")) %>%
        mutate(weight_units = weight_units %>%
                 # Extract inside parentheses
                 stringr::str_extract(., "(?<=\\().*(?=\\))"),
               weight = weight %>%
                 # Case where decimal number has extra space
                 gsub("\\. ", ".", .) %>%
                 suppressWarnings(as.numeric(.))) %>%
        # Grouped filtering to find weight values (remove duplicates)
        group_by(curator_comment) %>%
        filter(weight == max(weight, na.rm=TRUE)) %>%
        ungroup()
      
      # Run through cases to adjust/rename or split out
      fix_cols = names(tmp)[!names(tmp) %in% names(template[[s]])]
      
      # Run through cases to adjust/rename or split out
      fix_cols = names(tmp)[!names(tmp) %in% names(template[[s]])]
      
      if(length(fix_cols)) stop("Unhandled subjects field: ", toString(fix_cols))
      
    } else if(s == "Series") {
      
      # Some NTP studies have their time/units columns together, so don't map
      if(!any(grepl("time|dose", names(tmp), ignore.case=TRUE))){
        # Pull s_in_dat again, including missing time
        tmp = s_in_dat %>%
          dplyr::rename(any_of(s_map)) %>%
          select(any_of(names(s_map)), matches("target time|dose|concentration", ignore.case = TRUE)) %>%
          distinct()
      }
      
      # Some NTP studies do not have a "Test Article" or CASRN field which this maps for
      if(!"test_substance_name" %in% names(tmp)){
        tmp$test_substance_name = toString(intro_dat$value[intro_dat$field_name == "Compound Name"])
      }
      
      # Run through cases to adjust/rename or split out
      fix_cols = names(tmp)[!names(tmp) %in% names(template[[s]])] %>%
        .[!grepl("Concentration", .)] %>% #"Concentration \\(|Concentration Specification|\\) Concentration", .)] %>%
        .[!. %in% c("administration_route", "sex", "test_substance_name", "time", "dose_level", "dose_level_units",
                    "Study Time", "Study Time Unit",
                    # Check for already mapped columns (not the same name to and from)
                    map$to[map$sheet == tolower(s) & map$from != map$to] %>% unique()
        )]
      
      # Handle time level/units splitting
      f_col <- fix_cols[grepl("target time|time point", fix_cols, ignore.case = TRUE)]
      if(length(f_col)) {
        tmp = tmp %>%
          pivot_longer(cols=contains(c("target time", "time point"), ignore.case=TRUE), 
                       names_to = "time_units", values_to = "time") %>%
          mutate(time_units = stringr::str_extract(time_units, "(?<=\\().*(?=\\))"))
      }
      # Remove handled cases
      fix_cols <- fix_cols[!fix_cols %in% f_col]
      
      # Handle dose frequency/units splitting
      f_col <- fix_cols[grepl("dose frequency|dose_frequency", fix_cols, ignore.case = TRUE)]
      if(length(f_col)) {
        tmp = tmp %>%
          tidyr::pivot_longer(cols=all_of(f_col), names_to = "dose_frequency_units", values_to = "dose_frequency") %>%
          dplyr::mutate(dose_frequency_units = stringr::str_extract(dose_frequency_units, "(?<=\\().*(?=\\))")) %>%
          filter(!is.na(dose_frequency), !is.na(dose_frequency_units)) %>%
          tidyr::unite(col="dose_frequency", dose_frequency, dose_frequency_units, sep = " ") %>%
          dplyr::mutate(dose_frequency = gsub(" NA", "", dose_frequency) %>%
                          stringr::str_squish())
        tmp$dose_frequency[tmp$dose_frequency == "NA"] <- NA
      }
      # Remove handled cases
      fix_cols <- fix_cols[!fix_cols %in% f_col]
      
      # Handle dose level/units splitting
      f_col <- fix_cols[grepl("dose", fix_cols, ignore.case = TRUE)]
      if(length(f_col)) {
        tmp = tmp %>%
          pivot_longer(cols=all_of(f_col), names_to = "dose_level_units", values_to = "dose_level") %>%
          mutate(dose_level_units = stringr::str_extract(dose_level_units, "(?<=\\().*(?=\\))"))
      }
      # Remove handled cases
      fix_cols <- fix_cols[!fix_cols %in% f_col]
      
      if(length(fix_cols)) stop("Unhandled studies field: ", toString(fix_cols))
      
      # Figure name as Animal ID
      tmp$figure_name = paste0("Animal ID: ", tmp$figure_name, "_", tmp$sex)
      
      # Split columns based on conc_medium
      tmp = tmp %>%
        dplyr::mutate(across(any_of(c("dose_level", #"dose_frequency", 
                                      "dose_volume")), ~suppressWarnings(as.numeric(.)))) %>%
        # Filter out NA dose_levels
        filter(!is.na(dose_level)) %>%
        # Clean up and set default fields
        dplyr::mutate(figure_type = "Table",
                      log_conc_units = 0,
                      conc_cumulative = 0,
                      n_subjects_in_series = 1,
                      tmp_conc_id = 1:n())
      
      # Handle case of conc_medium in "()" of a field name
      # Create a Concentration Unit field
      if(any(grepl("Concentration \\(", names(tmp)))){
        
        # Get list of conc fields with units in the name
        conc_cols <- names(tmp)[grepl("Concentration \\(", names(tmp))]
        
        # Make names unique
        conc_cols_name <- sub('Concentration \\(.*', '', conc_cols[grepl("Concentration \\(", conc_cols)]) %>%
          stringr::str_squish() %>%
          make.unique(sep="_") %>%
          # Add additional suffix for cases where Conc and Unit field already present
          paste0(., "_a")
        
        # Get units
        conc_cols_units <- sub('.*\\(', '', conc_cols[grepl("Concentration \\(", conc_cols)]) %>%
          stringr::str_squish()
        # Splice back together
        conc_cols <- paste0(conc_cols_name, " Concentration (", conc_cols_units)
        # Set new unique names
        names(tmp)[grepl("Concentration \\(", names(tmp))] <- conc_cols #paste0(conc_cols_name, " (", conc_cols_units)
        
        # Create concentration units columns
        for(col in conc_cols){
          tmp_col <- col %>% 
            strsplit(split="\\(") %>% unlist()
          tmp_col[1] <- tmp_col[1] %>% 
            stringr::str_squish() %>%
            paste0(., " Unit")
          
          tmp[[tmp_col[1]]] <- tmp_col[2] %>% gsub("\\)", "", .)
        }
        
        # Remove units from conc name
        names(tmp)[grepl("Concentration \\(", names(tmp))] <- sub('\\(.*', '', 
                                                                  names(tmp)[grepl("Concentration \\(", names(tmp))]) %>%
          stringr::str_squish()
      }
      
      # Handle case of conc_medium in field name
      if(any(grepl("Concentration", names(tmp)))){
        tmp <- tmp %>%
          # Splitting out columns with units in the name
          tidyr::pivot_longer(names_to = "conc_medium", values_to = "conc_col_values", 
                              cols=contains("Concentration")
          ) %>%
          tidyr::separate(col=conc_medium, into=c("conc_medium", "conc_cols"), sep=" Concentration",
                          extra="merge") %>%
          dplyr::rowwise() %>%
          dplyr::mutate(conc_cols = ifelse(conc_cols == "", "conc", 
                                           ifelse(grepl("Unit", conc_cols), "conc_units", 
                                                  ifelse(grepl("Specification", conc_cols), 
                                                         "conc_curator_comment", conc_cols)
                                           )
          )) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(
            # id_cols = c(tmp_conc_id, figure_name, test_substance_name, 
            #             dose_level, dose_level_units, administration_route, 
            #             analyte_name, time_units, conc_medium), 
            names_from = "conc_cols", 
            values_from = "conc_col_values") %>%
          dplyr::mutate(conc_medium = gsub("Concentration", "", conc_medium),
                        conc_units = gsub("\\)", "", conc_units),
                        across(any_of(c("conc_medium", "conc_units", "conc_curator_comment")), ~stringr::str_squish(.))) %>%
          # Filter out those without units 
          filter(!is.na(conc_units))
      }
      
      # Fix concentration specification fields to match conc_medium entries
      # Used on conc_time_values sheet later as comments
      # Potentially deprecated???
      if(any(grepl("Concentration Specification", names(tmp)))){
        stop("Found case where 'Concentration Specification' is not handled...")
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
          select(-tmp_conc_id) %>%
          # Case where the Analyte is "NA" for a baseline measurement
          filter(analyte_name != 'NA') %>%
          mutate(dose_level = suppressWarnings(as.numeric(dose_level)))
      } else {
        tmp$conc_curator_comment = NA
      }
    }
    
    # Return distinct with sequential ID field
    tmp %>% 
      distinct() %>%
      # Add id sequence, with sheet and sheetname
      dplyr::mutate(id = paste0(1:n(), "_", s, "_", sheetname)) %>%
      select(-any_of(c("tmp_conc_id"))) %>%
      return()
  }) %T>% {
    names(.) <- names(template)[!names(template) %in% c("Conc_Time_Values")]
  }
  
  # Create conc_time_values data from splitting Series sheet
  out$Conc_Time_Values = out$Series %>%
    dplyr::select(fk_series_id = id, time, conc, figure_name, conc_curator_comment) %>%
    # Add id field
    dplyr::mutate(#time = as.numeric(time), 
      id = 1:n()) %>%
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
  
  return(out)
}