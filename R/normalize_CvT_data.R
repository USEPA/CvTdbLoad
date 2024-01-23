#' @description A helper function to orchestrate normalization of CvT Data
#' @title FUNCTION_TITLE
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [left_join][dplyr::left_join], [select][dplyr::select], [mutate][dplyr::mutate], [distinct][dplyr::distinct]
#' @rdname normalize_CvT_data
#' @export 
#' @importFrom dplyr left_join select mutate distinct
normalize_CvT_data <- function(df, f){
  df$Subjects = normalize_weight(raw=df$Subjects, f=f)
  df$Subjects = normalize_height(raw=df$Subjects, f=f)
  df$Subjects = normalize_age(raw=df$Subjects, f=f)
  #"SELECT c.id, s.id, c.fk_series_id, c.time_original, s.time_units_original, c.time_hr FROM series s LEFT JOIN conc_time_values c on s.id = c.fk_series_id"
  #Normalize time requires Series and Conc_Time_Values
  tmp = normalize_time(raw = df$Series %>% 
                         dplyr::left_join(df$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
                         dplyr::select(id, time_original=time, time_units_original=time_units), 
                       f = f)
  df$Conc_Time_Values = df$Conc_Time_Values %>% 
    dplyr::mutate(tempID = seq_len(nrow(df$Conc_Time_Values))) %>%
    dplyr::left_join(tmp, by=c("tempID", "fk_series_id"="id")) %>% 
    dplyr::select(-time, -time_units_original, -tempID)
  #Check radiolabel for evidence of radiolabeled chemicals or analytes
  check_radiolabel(raw=df$Series %>%
                     dplyr::select(analyte_name, analyte_name_secondary, fk_study_id, radiolabeled) %>%
                     dplyr::left_join(df$Studies %>% dplyr::select(id, test_substance_name), by=c("fk_study_id"="id")),
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
                         dplyr::left_join(df$Studies, by=c("fk_study_id"="id")) %>%
                         dplyr::left_join(df$Subjects, by=c("fk_subject_id"="id")) %>%
                         dplyr::select(fk_study_id, species, subtype, weight_kg, height_cm,
                                test_substance_name, dose_level, dose_level_units, dose_volume, administration_route_normalized),
                       f=f) %>%
    dplyr::select(fk_study_id, dose_level_normalized) %>% dplyr::distinct()
  df$Studies = df$Studies %>%
    dplyr::left_join(tmp, by=c("id"="fk_study_id"))
  #Normalize Conc Units
  tmp = normalize_conc(raw=df$Series %>%
                         dplyr::left_join(df$Subjects %>%
                                     dplyr::select(id, species), by=c("fk_study_id"="id")) %>%
                         dplyr::left_join(df$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
                         dplyr::select(id, species, conc_medium, analyte_name, analyte_name_secondary, analyte_casrn,
                                conc_original=conc, conc_units_original=conc_units,
                                conc_sd_original=conc_sd, conc_lower_bound_original=conc_lower_bound,
                                conc_upper_bound_original=conc_upper_bound, dsstox_substance_id=dsstox_substance_id), 
                             f=f) %>%
    dplyr::select(-conc_medium, -analyte_name, -analyte_name_secondary, -analyte_casrn, -conc_units_original)
  df$Conc_Time_Values = df$Conc_Time_Values %>%
    dplyr::mutate(tempID = seq_len(nrow(df$Conc_Time_Values))) %>%
    dplyr::select(-conc, -conc_sd, -conc_lower_bound, -conc_upper_bound) %>%
    dplyr::left_join(tmp, by=c("tempID")) %>%
    dplyr::select(-tempID)
  
  df$Studies[c("dose_duration", "dose_duration_units")] = normalize_dose_duration(df$Studies %>% dplyr::select(dose_duration, dose_duration_units))
  
  return(df)
}
