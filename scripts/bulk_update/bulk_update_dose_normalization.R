
bulk_update_dose_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT b.id, a.fk_study_id, ",
                  "e.chemical_name_original as test_substance_name, b.dose_level_original as dose_level, ",
                  "b.dose_level_units_original as dose_level_units, b.dose_volume, b.dose_volume_units, ",
                  "d.administration_route_normalized, b.fk_dosed_chemical_id, b.dose_level_normalized ",
                  "FROM cvt.studies b ",
                  "LEFT JOIN cvt.series a ON a.fk_study_id = b.id ",
                  "LEFT JOIN cvt.subjects c ON a.fk_subject_id = c.id ",
                  "LEFT JOIN cvt.administration_route_dict d ON b.fk_administration_route_id = d.id ",
                  "LEFT JOIN cvt.chemicals e ON b.fk_dosed_chemical_id = e.id ",
                  "WHERE b.dose_level_original is not null"
  )

  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(dose_old = dose_level_normalized) %>%
    dplyr::distinct()
  
  dups = df_raw %>%
    dplyr::count(id) %>%
    dplyr::filter(n > 1)
  
  if(nrow(dups)){
    stop("Duplicate ID values pulled from query...")
  }
  
  # Collapse ID field to improve conversion speeds (only convert unique cases once)
  df_raw_zip <- df_raw %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%  
    dplyr::summarise(id = toString(unique(id))) %>%
    dplyr::ungroup()
  
  # Normalized data
  df_normalized <- normalize_dose(raw=df_raw_zip)
  
  # Filter to those where "dose_old" does not equal the new "dose_level_normalized"
  compare <- df_normalized %>% 
    # Round to 3 decimal places to help with comparison
    dplyr::mutate(dplyr::across(c(dose_old, dose_level_normalized), ~round(as.numeric(.), 3))) %>%
    dplyr::filter(xor(is.na(dose_old), is.na(dose_level_normalized))|dose_old != dose_level_normalized)
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(dose_old), is.na(dose_level_normalized)) %>%
    dplyr::pull(dose_level_units) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(dose_old), !is.na(dose_level_normalized)) %>%
    dplyr::pull(dose_level_units) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Expand back out the ID field
  df_update <- df_normalized %>%
    tidyr::separate_rows(id, sep = ", ")
  
  if(nrow(df_update) != nrow(df_raw)){
    message("Error with normalization, input rows do not equal output...")
    browser()
    stop("Error with normalization, input rows do not equal output...")
  }
  
  # Filter to only entries that need updating
  df_out = df_update %>%
    dplyr::mutate(dose_level_normalized = as.character(dose_level_normalized)) %>%
    # Can't compare NA values, so replace for now
    tidyr::replace_na(list(dose_old = "-99999", dose_level_normalized = "-99999")) %>%
    dplyr::filter(dose_old != dose_level_normalized) %>%
    dplyr::filter(dose_level_normalized != "-99999") %>%
    dplyr::select(id, dose_level_normalized) %>%
    dplyr::distinct() %>%
    dplyr::mutate(qc_notes = "normalized dose updated")
  
  if(nrow(df_out)){
    # Push updated values to Studies sheet based on "id" field
    db_update_tbl(df=df_out,
                  tblName = "studies")  
  } else {
    message("No dose normalization updates to push...")
  }
  
  # Return updated values
  return(df_out)
}
