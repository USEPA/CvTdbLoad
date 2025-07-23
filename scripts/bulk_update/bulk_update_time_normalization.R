#' @title bulk_update_time_normalization
#' @description Utility function to perform bulk time normalization to all Conc_Time_Values records.
#' @return Dataframe log of updated records.
#' @seealso 
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{replace_na}}
#' @rdname bulk_update_time_normalization
#' @export 
#' @importFrom dplyr mutate group_by across summarise ungroup filter pull select distinct
#' @importFrom tidyr separate_rows replace_na
bulk_update_time_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT a.id, a.time_original, a.time_hr, b.time_units_original ",
                  "FROM cvt.conc_time_values a ",
                  "LEFT JOIN cvt.series b ON a.fk_series_id = b.id ",
                  "WHERE a.time_original is not NULL")
  
  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(time_old = time_hr)
  
  # Collapse ID field to improve conversion speeds (only convert unique cases once)
  df_raw_zip <- df_raw %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%  
    dplyr::summarise(id = toString(id)) %>%
    dplyr::ungroup()
  
  # Normalized data
  df_normalized <- normalize_time(raw=df_raw_zip)
  
  # Filter to those where "time_old" does not equal the new "time_hr"
  compare <- df_normalized %>% 
    # Round to 3 decimal places to help with comparison
    dplyr::mutate(dplyr::across(c(time_old, time_hr), ~round(as.numeric(.), 3))) %>%
    dplyr::filter(xor(is.na(time_old), is.na(time_hr))|time_old != time_hr)
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(time_old), is.na(time_hr)) %>%
    dplyr::pull(time_units_original) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(time_old), !is.na(time_hr)) %>%
    dplyr::pull(time_units_original) %>%
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
    tidyr::replace_na(list(time_old = -99999, time_hr = -99999)) %>%
    dplyr::filter(time_old != time_hr) %>%
    dplyr::filter(time_hr != -99999) %>%
    dplyr::select(id, time_hr) %>%
    dplyr::distinct() %>%
    dplyr::mutate(qc_notes = "normalized time updated")
  
  if(nrow(df_out)){
    # Push updated values to Conc_Time_Values sheet based on "id" field
    db_update_tbl(df=df_out,
                  tblName = "conc_time_values")  
  } else {
    message("No time normalization updates to push...")
  }
  
  # Return updated values
  return(df_out)
}
