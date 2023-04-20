# Script to automate extraction for NTP style CvT data documents
# Created by: Jonathan Taylor Wall
# Created Date: 2023-03-8
#Load packages
# require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl); library(httk)
# R version 4.1.0 (2021-05-18)
# httk_2.1.0; readxl_1.3.1; tidyr_1.2.0; magrittr_2.0.2; dplyr_1.0.8; DBI_1.1.2
# purrr_0.3.4; assertthat_0.2.1
#Load R Scripts
#' @title extract_ntp_data_file
#' @description Function to semi-automate extraction of NTP data to CvT template
#' @param filepath File path to the NTP file to extract to CvT template
#' @param template_path File path to latest template
#' @param template_map File path to a field name map from NTP to CvT template names
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [excel_sheets][readxl::excel_sheets], [read_xlsx][readxl::read_xlsx]
#'  [mutate][dplyr::mutate], [across][dplyr::across], [everything][dplyr::everything], [filter][dplyr::filter], [bind_rows][dplyr::bind_rows], [select][dplyr::select], [n][dplyr::n], [left_join][dplyr::left_join], [rename][dplyr::rename], [any_of][dplyr::any_of]
#'  [compact][purrr::compact], [pluck][purrr::pluck], [list_modify][purrr::list_modify], [map][purrr::map]
#'  [separate][tidyr::separate]
#' @rdname extract_ntp_data_file
#' @export 
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom dplyr mutate across everything filter bind_rows select n left_join rename any_of
#' @importFrom purrr compact pluck list_modify map
#' @importFrom tidyr separate
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
    # Return NULL if no data for a sheet
    if(!nrow(tmp)){
      return(NULL)
    }
    if(s != "Intro"){
      tmp = tmp %>%
        # Handle cases so bind_rows() can happen later
        dplyr::mutate(dplyr::across(dplyr::everything(), ~suppressWarnings(as.character(.))))
    }
    return(tmp)
  }) %T>% {
    names(.) <- s_list
  } %>%
    purrr::compact()
  
  # Get intro data separated and transformed into usable format
  intro_dat = in_dat %>% 
    purrr::pluck("Intro") %>%
    rbind(paste("Title:", names(.))) %T>% {
      names(.) <- "intro_metadata"
    } %>%
    dplyr::filter(!is.na(intro_metadata)) %>%
    tidyr::separate(intro_metadata, into=c("field_name", "value"), 
                    sep=": ", 
                    fill = "right")
  
  # Remove intro sheet
  in_dat = in_dat %>%
    purrr::list_modify("Intro" = NULL)

  in_dat = lapply(in_dat, function(s){
    s %>%
      # Filter out caption information
      dplyr::filter(!grepl(paste0(c("BLOQ",
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
  
  # Pull templates to map fields
  template = get_cvt_template(template_path = template_path)
  map = readxl::read_xlsx(template_map)
  
  # Loop through each in_dat sheet and process into template
  out <- lapply(names(in_dat), function(s){
    format_ntp_template(s_in_dat = in_dat[[s]], 
                        map=map, 
                        template=template,
                        sheetname=s
                        )
  }) %T>% {
    names(.) <- names(in_dat)
  }
  
  # Empty dataframe for key map
  id_map <- data.frame()
  # Combine sheets into single template (handle ID values)
  out <- lapply(names(template), function(s){
    tmp <- out %>%
      purrr::map(s) %>%
      dplyr::bind_rows()
    # Build key map
    if(!s %in% c("Conc_Time_Values")){
      id_map <<- tmp %>%
        dplyr::select(id) %>%
        unique() %>%
        dplyr::mutate(sheet=s,
               id_new = 1:dplyr::n()) %>%
        rbind(id_map, .)
      # Renumber ID column
      tmp <- tmp %>%
        dplyr::mutate(id = 1:dplyr::n())
    }
    return(tmp)
  }) %T>% {
    names(.) <- names(template)
  }
  
  # Remap Foreign Key ID fields
  # fk_study_id remap
  out$Series <- out$Series %>%
    dplyr::left_join(id_map %>% 
                dplyr::filter(sheet == "Studies") %>%
                dplyr::select(-sheet),
              by=c("fk_study_id"="id")) %>%
    dplyr::select(-fk_study_id) %>%
    dplyr::rename(fk_study_id=id_new)
  # fk_subject_id remap
  out$Series <- out$Series %>%
    dplyr::left_join(id_map %>% 
                       dplyr::filter(sheet == "Subjects") %>%
                       dplyr::select(-sheet),
                     by=c("fk_subject_id"="id")) %>%
    dplyr::select(-fk_subject_id) %>%
    dplyr::rename(fk_subject_id=id_new) %>%
    # Reorder columns
    dplyr::select(dplyr::any_of(names(template$Series)))
  # fk_series_id remap
  out$Conc_Time_Values <- out$Conc_Time_Values %>%
    dplyr::left_join(id_map %>% 
                       dplyr::filter(sheet == "Series") %>%
                       dplyr::select(-sheet),
                     by=c("fk_series_id"="id")) %>%
    dplyr::select(-fk_series_id) %>%
    dplyr::rename(fk_series_id=id_new) %>%
    dplyr::select(dplyr::any_of(names(template$Conc_Time_Values)))
  
  
  # De-Dup entries (Documents and Studies)
  # Documents sheet is always going to be all dups of the first row
  # doc_dups <- get_ntp_dup_keys(df=out, sheet="Documents")
  out$Documents <- out$Documents[1, ]
  # Studies will have duplicates, so get dataframe of dups
  study_dups <- get_ntp_dup_keys(df=out, sheet="Studies")
  # Remove duplicates from studies
  out$Studies <- out$Studies[!duplicated(out$Studies %>% dplyr::select(-id)),]
  # Remap dups in Series sheet
  out$Series <- out$Series %>%
    dplyr::left_join(study_dups, 
              by=c("fk_study_id"="dup_id")) %>%
    dplyr::select(-fk_study_id) %>%
    dplyr::rename(fk_study_id=id_new) %>%
    dplyr::select(dplyr::any_of(names(template$Series)))
  
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
