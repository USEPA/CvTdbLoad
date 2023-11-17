#' @description Function to check if processed document is missing required fields.
#' @param df List of dataframes for the sheets within an extraction template
#' @param f Filename for flagging purposes #'
#' @return None. Logs any flags
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter]
#' @rdname check_required_fields
#' @export 
#' @importFrom dplyr filter
check_required_fields <- function(df, f){
  for(t in names(df)){
    req_fields = switch(t,
                        "Documents" = c("id", "document_type", "extracted", 
                                        "tk_params", "effects_data", "httk_data"),
                        "Studies" = c("id", "dose_level", "administration_route_normalized",
                                      "dose_frequency"),
                        "Subjects" = c("id", "species"),
                        "Series" = c("id", "radiolabeled", "figure_name", "time_units_original", 
                                     "conc_units_original", "conc_medium_normalized",
                                     "fk_study_id", "fk_subject_id", "n_subjects_in_series"),
                        "Conc_Time_Values" = c("fk_series_id", "time_original", "conc")
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
        # #Check if dose_duration present (if dose_frequency present)
        # #if the dose frequency is "1" i guess you wouldn't need a dose duration - Risa Sayre 2021-12-1
        # tmp = df[[t]] %>%
        #   dplyr::filter(dose_frequency != 1)
        # if(any(!is.na(tmp$dose_frequency)) & all(is.na(tmp$dose_duration))){
        #   log_CvT_doc_load(f=f, m="dose_frequency_present_without_dose_duration")
        # }
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
      
      if(all(c("dose_volume", "dose_volume_units") %in% names(df[[t]]))){
        #Check if dose_volume_units present (if dose_volume present)
        if(any(!is.na(df[[t]]$dose_volume)) & all(is.na(df[[t]]$dose_volume_units))){
          log_CvT_doc_load(f=f, m="missing_dose_volume_units")
        }
      }
      
      if(all(c("dermal_applied_area", "dermal_applied_area_units") %in% names(df[[t]]))){
        #Check if dermal_applied_area_units present (if dermal_applied_area present)
        if(any(!is.na(df[[t]]$dermal_applied_area)) & all(is.na(df[[t]]$dermal_applied_area_units))){
          log_CvT_doc_load(f=f, m="missing_dermal_applied_area_units")
        }
      }
      
      if(all(c("aerosol_particle_density", "aerosol_particle_density_units") %in% names(df[[t]]))){
        #Check if aerosol_particle_density_units present (if aerosol_particle_density present)
        if(any(!is.na(df[[t]]$aerosol_particle_density)) & all(is.na(df[[t]]$aerosol_particle_density_units))){
          log_CvT_doc_load(f=f, m="missing_aerosol_particle_density_units")
        }
      }
        
      #dose duration is required for inhalation/dermal
      #If dose_frequency is not 1 or NULL, then we need a dose_duration
      #although, we have logic that if dose_frequency is present, they must have dose_duration...so simplifying
      tmp = df[[t]] %>% dplyr::filter((administration_route_normalized %in% c("inhalation", "dermal") | grepl("infusion", administration_route_original)),
                               is.na(dose_duration) )
      if(nrow(tmp)){
        log_CvT_doc_load(f=f, m="missing_dose_duration_for_inhalation_dermal_iv_infusion_study")
      }
    }
    
    if(t == "Subjects"){
      if(all(c("age", "age_units") %in% names(df[[t]]))){
        #Check if age_units present (if age present)
        if(any(!is.na(df[[t]]$age)) & all(is.na(df[[t]]$age_units))){
          log_CvT_doc_load(f=f, m="missing_age_units")
        }
      }
      
      if(all(c("height", "height_units") %in% names(df[[t]]))){
        #Check if height_units present (if height present)
        if(any(!is.na(df[[t]]$height)) & all(is.na(df[[t]]$height_units))){
          log_CvT_doc_load(f=f, m="missing_height_units")
        }
      }
      
      if(all(c("weight", "weight_units") %in% names(df[[t]]))){
        #Check if weight_units present (if weight present)
        if(any(!is.na(df[[t]]$weight)) & all(is.na(df[[t]]$weight_units))){
          log_CvT_doc_load(f=f, m="missing_weight_units")
        }
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
    
    if(t == "Conc_Time_Values"){
      #Check if conc_bound_type present (if conc lower, upper, or sd present)
      if((any(!is.na(df[[t]]$conc_upper_bound)) | any(is.na(df[[t]]$conc_lower_bound)) | any(!is.na(df[[t]]$conc_sd))) & 
         all(is.na(df[[t]]$conc_bound_type))){
        log_CvT_doc_load(f=f, m="missing_conc_bound_type")
      }
    }
  }
}
