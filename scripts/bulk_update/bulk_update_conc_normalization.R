
bulk_update_conc_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT distinct ",
                  "b.id, b.fk_series_id, a.conc_units_original, ",
                  "b.conc_original, b.conc, b.conc_sd_original, b.conc_lower_bound_original, b.conc_upper_bound_original, ",
                  "c.chemical_name_original, c.chemical_name_secondary_original, c.casrn_original, c.id as fk_analyzed_chemical_id, ",
                  "d.conc_medium_normalized, d.units as desired_units, a.conc_units_normalized as conc_units_normalized_old, ",
                  "f.test_environment_temperature, a.radiolabeled, ",
                  "e.species ",
                  "FROM cvt.series a ",
                  "LEFT JOIN cvt.conc_time_values b ON a.id = b.fk_series_id ",
                  "LEFT JOIN cvt.chemicals c ON c.id = a.fk_analyzed_chemical_id ",
                  "LEFT JOIN cvt.conc_medium_dict d ON d.id = a.fk_conc_medium_id ",
                  "LEFT JOIN cvt.subjects e ON a.fk_subject_id = e.id ",
                  "LEFT JOIN cvt.studies f ON a.fk_study_id = f.id ",
                  "WHERE b.conc_original IS NOT NULL AND d.conc_medium_normalized IS NOT NULL AND ",
                  "d.units IS NOT NULL"
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
  
  # Check radiolabeling need
  radiolabel_check = df_raw_zip %>%
    dplyr::select(chemical_name_original, chemical_name_secondary_original, radiolabeled) %>%
    dplyr::distinct() %>%
    dplyr::filter(is.na(radiolabeled)) %>%
    dplyr::mutate(
      analyte_name_check = chemical_name_original,
      analyte_name_secondary_check = chemical_name_secondary_original,
      dplyr::across(
        .cols = c(analyte_name_check, analyte_name_secondary_check),
        .fns = ~ case_when(
          # Check contains [14] (often [14]C)
          grepl("\\[14\\]", .) ~ TRUE,
          # Check contains [3] (often [3]H)
          grepl("\\[3\\]", .) ~ TRUE,
          # If contains a double digit, could be a radiolabeled substance - check
          grepl("([1-9][0-9])", .) ~ TRUE,
          TRUE ~ FALSE  
        )
      )
    ) %>%
    dplyr::filter(analyte_name_check == TRUE | analyte_name_secondary_check == TRUE)
  
  View(radiolabel_check, title = "radiolabel_check")
  # Normalized data
  df_normalized <- normalize_conc(raw=df_raw_zip) %>%
    dplyr::mutate(
      # Set as desired_units from conc_medium_dict table
      conc_units_normalized = desired_units
    )
  
  # Report missing conversions
  message("Missing conversion logic: ")
  df_normalized %>% 
    dplyr::filter(grepl("No conversion", conv_equ_raw)) %>%
    dplyr::pull(conv_equ_raw) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
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
  
  compare_norm_units = df_update %>%
    dplyr::mutate(compare = dplyr::case_when(
      is.na(conc_units_normalized_old) & is.na(conc_units_normalized) ~ TRUE,
      !is.na(conc_units_normalized_old) & is.na(conc_units_normalized) ~ FALSE,
      is.na(conc_units_normalized_old) & !is.na(conc_units_normalized) ~ FALSE,
      TRUE ~ conc_units_normalized_old == conc_units_normalized
    )) %>%
    dplyr::filter(compare == FALSE) %>%
    dplyr::select(fk_series_id, conc_units_normalized, conc_units_normalized_old, desired_units) %>%
    dplyr::distinct()
  
  # Filter to only entries that need updating
  df_out = df_update %>%
    # Can't compare NA values, so replace for now
    tidyr::replace_na(list(conc_old = "-99999", conc = "-99999")) %>%
    dplyr::filter(conc_old != conc) %>%
    dplyr::filter(conc != -99999) %>%
    dplyr::select(id, fk_series_id, conc, conc_sd, conc_lower_bound, conc_upper_bound, conc_units_normalized) %>%
    dplyr::distinct()
  
  if(nrow(df_out)){
    # Push updated values to Conc_Time_Values sheet based on "id" field
    db_update_tbl(df=df_out %>%
                    dplyr::select(-conc_units_normalized) %>%
                    dplyr::mutate(qc_notes = "normalized conc updated"),
                  tblName = "conc_time_values")  
  } else {
    message("No conc normalization updates to push")
  }
  
  if(nrow(compare_norm_units)){
    # Push updated conc_units_normalized
    db_update_tbl(df = compare_norm_units %>%
                    dplyr::select(id = fk_series_id, conc_units_normalized) %>%
                    dplyr::distinct() %>%
                    dplyr::mutate(qc_notes = "conc_units_normalized updated"),
                  tblName = "series")  
  } else {
    message("No conc unit normalization updates to push")
  }
  
  # Return updated values
  return(df_out)
}
