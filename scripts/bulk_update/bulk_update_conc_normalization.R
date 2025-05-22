
bulk_update_conc_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT distinct ",
                  "b.id, a.conc_units_original, ",
                  "b.conc_original, b.conc, b.conc_sd_original, b.conc_lower_bound_original, b.conc_upper_bound_original, ",
                  "c.chemical_name_original, c.chemical_name_secondary_original, c.casrn_original, c.id as fk_analyzed_chemical_id, ",
                  "d.conc_medium_normalized, ",
                  "e.species ",
                  "FROM cvt.series a ",
                  "LEFT JOIN cvt.conc_time_values b ON a.id = b.fk_series_id ",
                  "LEFT JOIN cvt.chemicals c ON c.id = a.fk_analyzed_chemical_id ",
                  "LEFT JOIN cvt.conc_medium_dict d ON d.id = a.fk_conc_medium_id ",
                  "LEFT JOIN cvt.subjects e ON a.fk_subject_id = e.id ",
                  "WHERE b.conc_original IS NOT NULL"
  )

  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(conc_old = conc,
                  conc_medium = conc_medium_normalized)
    
  # Collapse ID field to improve conversion speeds (only convert unique cases once)
  df_raw_zip <- df_raw %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%  
    dplyr::summarise(id = toString(id)) %>%
    dplyr::ungroup()
  
  # Normalized data
  df_normalized <- normalize_conc(raw=df_raw_zip)
  
  # Filter to those where "conc_old" does not equal the new "conc"
  compare <- df_normalized %>% 
    # Round to 3 decimal places to help with comparison
    dplyr::mutate(dplyr::across(c(conc_old, conc), ~round(as.numeric(.), 3))) %>%
    dplyr::filter(xor(is.na(conc_old), is.na(conc))|conc_old != conc)
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(conc_old), is.na(conc)) %>%
    dplyr::pull(conc_units_original) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(conc_old), !is.na(conc)) %>%
    dplyr::pull(conc_units_original) %>%
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
    # Can't compare NA values, so replace for now
    tidyr::replace_na(list(conc_old = "-99999", conc = "-99999")) %>%
    dplyr::filter(conc_old != conc) %>%
    dplyr::filter(conc != -99999) %>%
    dplyr::select(id, conc, conc_sd, conc_lower_bound, conc_upper_bound) %>%
    dplyr::distinct() %>%
    dplyr::mutate(qc_notes = "normalized conc updated")
  
  if(nrow(df_out)){
    # Push updated values to Conc_Time_Values sheet based on "id" field
    db_update_tbl(df=df_out,
                  tblName = "conc_time_values")  
  } else {
    message("No conc normalization updates to push")
  }
  
  # Return updated values
  return(df_out)
}
