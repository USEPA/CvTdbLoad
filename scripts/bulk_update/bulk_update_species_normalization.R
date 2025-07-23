#' @title bulk_update_species_normalization
#' @description Utility function to perform bulk subject species normalization to all subject records.
#' @return Dataframe log of updated records.
#' @seealso 
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{replace_na}}
#' @rdname bulk_update_species_normalization
#' @export 
#' @importFrom dplyr mutate group_by across summarise ungroup filter pull select distinct
#' @importFrom tidyr separate_rows replace_na
bulk_update_species_normalization <- function(){
  # Set global variable for debugging (skip logging)
  ENV_DEBUG <<- TRUE
  API_AUTH <<- Sys.getenv("API_AUTH")
  
  # Query with needed columns
  query <- paste0("SELECT id, species FROM cvt.subjects WHERE species IS NOT NULL")
  
  # Pull data to check
  df_raw <- db_query_cvt(query) %>%
    # Preserve previous conversion value
    dplyr::mutate(species_old = species)
  
  # Collapse ID field to improve conversion speeds (only convert unique cases once)
  df_raw_zip <- df_raw %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%  
    dplyr::summarise(id = toString(id)) %>%
    dplyr::ungroup()
  
  # Normalized data
  df_normalized <- normalize_species(x=df_raw_zip)
  
  # Filter to those where "species_old" does not equal the new "species"
  compare <- df_normalized
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(species_old), is.na(species)) %>%
    dplyr::pull(species_old) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(species_old), !is.na(species)) %>%
    dplyr::pull(species) %>%
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
    tidyr::replace_na(list(species_old = "-99999", species = "-99999")) %>%
    dplyr::filter(species_old != species) %>%
    dplyr::filter(species != "-99999") %>%
    dplyr::select(id, species) %>%
    dplyr::distinct() %>%
    dplyr::mutate(qc_notes = "normalized species updated")
  
  if(nrow(df_out)){
    # Push updated values to Subjects sheet based on "id" field
    db_update_tbl(df=df_out,
                  tblName = "subjects")  
  } else {
    message("No species normalization updates to push...")
  }
  
  # Return updated values
  return(df_out)
}
