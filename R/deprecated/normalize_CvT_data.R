#' @description A helper function to orchestrate normalization of CvT Data
#' @title FUNCTION_TITLE
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param log_path File path where to save the log file.
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
normalize_CvT_data <- function(df, f, log_path){
  df$Subjects = normalize_weight(raw=df$Subjects, f=f, log_path=log_path)
  df$Subjects = normalize_height(raw=df$Subjects, f=f, log_path=log_path)
  df$Subjects = normalize_age(raw=df$Subjects, f=f, log_path=log_path)
  # "SELECT c.id, s.id, c.fk_series_id, c.time_original, s.time_units_original, c.time_hr FROM series s LEFT JOIN conc_time_values c on s.id = c.fk_series_id"
  # Normalize time requires Series and Conc_Time_Values
  tmp = normalize_time(raw = df$Series %>% 
                         dplyr::mutate(fk_series_id = id) %>%
                         dplyr::left_join(df$Conc_Time_Values, 
                                          by=c("fk_series_id")) %>%
                         dplyr::rename(any_of(c("time_original"="time", "time_units_original"="time_units"))) %>%
                         dplyr::select(id, fk_series_id, time_original, time_units_original), 
                       f = f, 
                       log_path=log_path)
  
  df$Conc_Time_Values = df$Conc_Time_Values %>% 
    dplyr::select(-any_of(c("time_hr"))) %>%
    dplyr::left_join(tmp %>%
                       dplyr::select(id, time_hr),
                     by="id")
  
  # Check radiolabel for evidence of radiolabeled chemicals or analytes
  check_radiolabel(raw=df$Series %>%
                     dplyr::rename(any_of(c("analyte_name"="analyte_name_original", 
                                            "analyte_name_secondary"="analyte_name_secondary_original"))) %>%
                     dplyr::select(id, analyte_name, analyte_name_secondary, fk_study_id, radiolabeled) %>%
                     dplyr::left_join(df$Studies %>% 
                                        dplyr::rename(any_of(c("test_substance_name"="test_substance_name_original"))) %>%
                                        dplyr::select(id, test_substance_name), 
                                      by=c("fk_study_id"="id")),
                   f=f,
                   log_path=log_path) # Combine Study and Series chemical information
  
  # Convert to boolean (assumes NA = 0, else is 1)
  df$Series = normalize_boolean(x=df$Series, col=c("radiolabeled", "log_conc_units"))
  
  # Convert tp character
  if(!is.character(df$Series$n_subjects_in_series)){
    df$Series$n_subjects_in_series = as.character(df$Series$n_subjects_in_series)  
  }
  
  # Harmonize conc_medium - Deprecated with new load approach
  # df$Series=normalize_conc_medium(raw=df$Series, f=f)
  
  # Normalize Dose
  tmp = normalize_dose(raw = df$Series %>%
                         dplyr::left_join(df$Studies, by=c("fk_study_id"="id")) %>%
                         dplyr::left_join(df$Subjects, by=c("fk_subject_id"="id")) %>%
                         dplyr::rename(any_of(c(
                           "test_substance_name" = "test_substance_name_original",
                           "dose_level" = "dose_level_original",
                           "dose_level_units" = "dose_level_units_original"
                         ))) %>%
                         dplyr::select(id, fk_study_id, species, subtype, weight_kg, height_cm,
                                       test_substance_name, dose_level, dose_level_units, 
                                       dose_volume, administration_route_normalized, fk_dosed_chemical_id),
                       f=f,
                       log_path = log_path) %>%
    dplyr::select(fk_study_id, dose_level_normalized) %>% 
    dplyr::distinct()
  
  df$Studies = df$Studies %>%
    dplyr::select(!any_of(c("dose_level_normalized"))) %>%
    dplyr::left_join(tmp, by=c("id"="fk_study_id"))
  
  #Normalize Conc Units
  tmp = normalize_conc(raw=df$Series %>%
                         dplyr::mutate(fk_series_id = id) %>%
                         dplyr::left_join(df$Subjects %>%
                                            dplyr::select(id, species), 
                                          by=c("fk_subject_id"="id")) %>%
                         dplyr::left_join(df$Conc_Time_Values, 
                                          by="fk_series_id") %>%
                          dplyr::rename(any_of(c(
                            "conc_original"="conc", "conc_units_original"="conc_units",
                            "conc_sd_original"="conc_sd", "conc_lower_bound_original"="conc_lower_bound",
                            "conc_upper_bound_original"="conc_upper_bound"
                          ))) %>%
                         #dplyr::rename(any_of(c(
                        #   "conc_medium" = "conc_medium_normalized",
                        #   "analyte_name"="analyte_name_original",
                        #   "analyte_name_secondary"="analyte_name_secondary_original",
                        #   "analyte_casrn_secondary"="analyte_casrn_original"
                        # ))) %>%
                         dplyr::select(id, fk_series_id, species, conc_medium, analyte_name, analyte_name_secondary, analyte_casrn,
                                       conc_original, conc_units_original,
                                       conc_sd_original, conc_lower_bound_original,
                                       conc_upper_bound_original),#, fk_analyzed_chemical_id), 
                       f=f,
                       log_path=log_path)
  
  df$Conc_Time_Values = df$Conc_Time_Values %>%
    dplyr::select(-conc, -conc_sd, -conc_lower_bound, -conc_upper_bound) %>%
    dplyr::left_join(tmp %>%
                       dplyr::select(
                         id, conc, conc_sd, conc_lower_bound, conc_upper_bound
                       ), 
                     by="id")
  
  # Deprecated with new load approach
  # df$Studies[c("dose_duration", "dose_duration_units")] = normalize_dose_duration(df$Studies %>% dplyr::select(dose_duration, dose_duration_units))
  
  return(df)
}