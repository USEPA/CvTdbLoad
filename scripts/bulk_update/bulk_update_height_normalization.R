
bulk_update_height_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT * FROM cvt.subjects WHERE height is not NULL")
  
  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(height_old = height_cm)
  
  # Collapse ID field to improve conversion speeds (only convert unique cases once)
  df_raw_zip <- df_raw %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%  
    dplyr::summarise(id = toString(id)) %>%
    dplyr::ungroup()
  
  # Normalized data
  df_normalized <- normalize_height(raw=df_raw_zip)
  
  # Filter to those where "height_old" does not equal the new "height_cm"
  compare <- df_normalized %>% 
    # Round to 3 decimal places to help with comparison
    dplyr::mutate(dplyr::across(c(height_old, height_cm), ~round(as.numeric(.), 3))) %>%
    dplyr::filter(xor(is.na(height_old), is.na(height_cm))|height_old != height_cm)
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(height_old), is.na(height_cm)) %>%
    dplyr::pull(height_units) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(height_old), !is.na(height_cm)) %>%
    dplyr::pull(height_units) %>%
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
    tidyr::replace_na(list(height_old = -99999, height_cm = -99999)) %>%
    dplyr::filter(height_old != height_cm) %>%
    dplyr::filter(height_cm != -99999) %>%
    dplyr::select(id, height_cm) %>%
    dplyr::distinct() %>%
    dplyr::mutate(qc_notes = "normalized height updated",
                  height_cm = round(height_cm, 3))
  
  if(nrow(df_out)){
    # Push updated values to Subjects sheet based on "id" field
    db_update_tbl(df=df_out,
                  tblName = "subjects")  
  } else {
    message("No height normalization updates to push...")
  }
  
  # Return updated values
  return(df_out)
}
