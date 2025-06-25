
bulk_update_age_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT * FROM cvt.subjects WHERE age is NOT NULL and age_category is NULL")
  
  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(age_category_old = age_category)
  
  # Collapse ID field to improve conversion speeds (only convert unique cases once)
  df_raw_zip <- df_raw %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%  
    dplyr::summarise(id = toString(id)) %>%
    dplyr::ungroup()
  
  # Normalized data
  df_normalized <- normalize_age(raw=df_raw_zip)
  
  # Filter to those where "age_category_old" does not equal the new "age_category"
  compare <- df_normalized
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(age_category_old), is.na(age_category)) %>%
    dplyr::pull(age_category_old) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(age_category_old), !is.na(age_category)) %>%
    dplyr::pull(age_category) %>%
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
    dplyr::mutate(age_category = as.character(age_category)) %>%
    # Can't compare NA values, so replace for now
    tidyr::replace_na(list(age_category_old = "-99999", age_category = "-99999")) %>%
    dplyr::filter(age_category_old != age_category) %>%
    dplyr::filter(age_category != "-99999") %>%
    dplyr::select(id, age_category) %>%
    dplyr::distinct() %>%
    dplyr::mutate(qc_notes = "normalized age_category updated")
  
  if(nrow(df_out)){
    # Push updated values to Subjects sheet based on "id" field
    db_update_tbl(df=df_out,
                  tblName = "subjects")  
  } else {
    message("No age_category normalization updates to push...")
  }
  
  # Return updated values
  return(df_out)
}
