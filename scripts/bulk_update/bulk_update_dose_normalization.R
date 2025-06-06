
bulk_update_dose_normalization <- function(report.only=TRUE){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT b.id, a.fk_study_id as series_fk_study_id, ",
                  "e.chemical_name_original as test_substance_name, ",
                  "b.dose_level_original as dose_level, b.dose_level_units_original as dose_level_units, ",
                  "b.dose_level_target, b.dose_level_target_units, ",
                  "b.dose_volume, b.dose_volume_units, ",
                  "b.fk_dosed_chemical_id, b.dose_level_normalized, b.dose_level_units_normalized as dose_level_units_normalized_old, ",
                  "b.administration_route_original, b.administration_method_original, b.administration_form_original, ",
                  "d.administration_route_normalized, f.administration_method_normalized, g.administration_form_normalized, ",
                  # Feed and drinking water studies need subject weight (kg) and administration_term (days)
                  "c.species, c.weight_kg, b.administration_term, b.administration_term_units ",
                  "FROM cvt.studies b ",
                  "LEFT JOIN cvt.series a ON a.fk_study_id = b.id ",
                  "LEFT JOIN cvt.subjects c ON a.fk_subject_id = c.id ",
                  "LEFT JOIN cvt.administration_route_dict d ON b.fk_administration_route_id = d.id ",
                  "LEFT JOIN cvt.administration_method_dict f ON b.fk_administration_method_id = f.id ",
                  "LEFT JOIN cvt.administration_form_dict g ON b.fk_administration_form_id = g.id ",
                  "LEFT JOIN cvt.chemicals e ON b.fk_dosed_chemical_id = e.id ",
                  "WHERE b.dose_level_original is not null ",
                  # Temporarily not normalizing dermal route doses - figuring out best units and conversions
                  "AND d.administration_route_normalized not in ('dermal')"
  )

  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(dose_old = dose_level_normalized) %>%
    dplyr::distinct() %>%
    # Collapse multiple subjects
    tidyr::unite(col="subject_info",
                 sep=": ",
                 species, weight_kg
                 ) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(subject_info = toString(subject_info)) %>%
    dplyr::ungroup() %>%
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
  df_normalized <- normalize_dose(raw=df_raw_zip) %>%
    dplyr::mutate(
      # Set as desired_units from conc_medium_dict table
      dose_level_units_normalized = desired_units
    )
  
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
  
  compare_norm_units = df_update %>%
    dplyr::mutate(compare = dplyr::case_when(
      is.na(dose_level_units_normalized_old) & is.na(dose_level_units_normalized) ~ TRUE,
      !is.na(dose_level_units_normalized_old) & is.na(dose_level_units_normalized) ~ FALSE,
      is.na(dose_level_units_normalized_old) & !is.na(dose_level_units_normalized) ~ FALSE,
      TRUE ~ dose_level_units_normalized_old == dose_level_units_normalized
    )) %>%
    dplyr::filter(compare == FALSE) %>%
    dplyr::select(id, dose_level_units_normalized, dose_level_units_normalized_old, dose_level_units_normalized) %>%
    dplyr::distinct()
  
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
  
  # Only push updates if report.only is FALSE
  if(!report.only){
    if(nrow(df_out)){
      # Push updated values to Studies sheet based on "id" field
      db_update_tbl(df=df_out,
                    tblName = "studies")  
    } else {
      message("No dose normalization updates to push...")
    }
    
    if(nrow(compare_norm_units)){
      # Push updated conc_units_normalized
      db_update_tbl(df = compare_norm_units %>%
                      dplyr::select(id, dose_level_units_normalized) %>%
                      dplyr::distinct() %>%
                      dplyr::mutate(qc_notes = "dose_level_units_normalized updated"),
                    tblName = "studies")
    } else {
      message("No conc unit normalization updates to push")
    }  
  } else {
    # Return updated values
    return(df_out) 
  }
}
