bulk_check_normalization <- function(f="debug_file", log_path="output/debug_log.xlsx"){
  ENV_DEBUG <<- TRUE
    
  # TODO Check for normalize_conc_units, normalize_boolean
  # TODO Update all functions to find the best place to return out$raw, before its NULLed
  query_list <- list(
      normalize_weight = paste0("SELECT distinct weight, weight_units ",
                                  "FROM cvt.subjects WHERE weight_kg is null"),
      normalize_height = paste0("SELECT distinct height, height_units ",
                                  "FROM cvt.subjects WHERE height_cm is null"),
      normalize_time = paste0("SELECT distinct a.time_original, b.time_units_original ",
                                "FROM cvt.conc_time_values a ",
                                 "LEFT JOIN cvt.series b ",
                                 "ON b.id = a.fk_series_id ",
                                 "WHERE a.time_hr is null"),
      normalize_dose = paste0("SELECT distinct dose_level_original as dose_level, dose_level_units_original as dose_level_units ",
                                "FROM cvt.studies WHERE dose_level_normalized is null"),
      normalize_conc = paste0("SELECT distinct b.id,
                                a.conc_units_original, a.fk_conc_medium_id,
                                b.conc_original, b.conc, b.conc_sd_original, b.conc_lower_bound_original, b.conc_upper_bound_original,
                                c.chemical_name_original, c.chemical_name_secondary_original, c.casrn_original, c.dsstox_substance_id,
                                d.conc_medium_normalized
                                FROM cvt.series a
                                LEFT JOIN cvt.conc_time_values b
                                ON a.id = b.fk_series_id
                                LEFT JOIN cvt.chemicals c
                                ON c.id = a.fk_analyzed_chemical_id
                                LEFT JOIN cvt.conc_medium_dict d
                                ON d.id = a.fk_conc_medium_id
                                WHERE conc is null")
      # Skip age normalization since we don't currently normalized in database
      #normalize_age = paste0("SELECT distinct age, age_units ",
      #                       "FROM cvt.subjects WHERE age_normalized is null"),
    )
  
  summary_list <- list()
  
  # Loop through each module
  for(normalization_name in names(query_list)){
    if (is.na(debug_list[[module]])){
      next
    }
    
    normalization_query = query_list[[normalization_name]]
    query_results <- db_query_cvt(normalization_query)
    
    # Return the out$raw "unhandled" cases
    normalization_function <- match.fun(normalization_name)
    summary <- normalization_function(raw=query_results, f=f, log_path=log_path, debug=TRUE)
    
    # Select appropriate columns for the summary
    if (normalization_name == "normalize_weight") {
      columns <- c("weight", "weight_units")
    } else if (normalization_name == "normalize_height") {
      columns <- c("height", "height_units")
    } else if (normalization_name == "normalize_time") {
      columns <- c("time_original", "time_units_original")
    } else if (normalization_name == "normalize_dose") {
      columns <- c("dose_level", "dose_level_units") 
    } else if (normalization_name == "normalize_conc") {
      columns <- c("conc_original", "conc_units_original", "conc_lower_bound_original", "conc_upper_bound_original")
    }
      
    # Generate a summary by module with unhandled units
    # Store the summary for later XLSX output, separated by sheet
    summary_list[[normalization_name]] <- summary %>% 
      select(all_of(columns)) %>%
      unique()
  }
  
  # Output XLSX from dataframe list
  writexl::write_xlsx(summary_list, "output/normalization_summary.xlsx")
}