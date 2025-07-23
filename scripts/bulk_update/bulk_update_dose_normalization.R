#' @title bulk_update_dose_normalization
#' @description Utility function to perform bulk dose normalization to all study records.
#' @param report.only Boolean whether to make updates or just report potential changes, Default: TRUE
#' @return Dataframe log of updated records.
#' @seealso 
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}
#'  \code{\link[tidyr]{unite}}, \code{\link[tidyr]{separate_rows}}, \code{\link[tidyr]{replace_na}}
#' @rdname bulk_update_dose_normalization
#' @export 
#' @importFrom dplyr mutate distinct group_by ungroup count filter across summarise rename pull case_when select all_of
#' @importFrom tidyr unite separate_rows replace_na
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
    dplyr::mutate(dose_level_normalized_old = dose_level_normalized) %>%
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
    dplyr::rename(dose_level_target_old = dose_level_target, 
                  dose_level_target_units_old = dose_level_target_units,
                  dose_level_old = dose_level,
                  dose_level_units_old = dose_level_units)
  
  # Filter to those where "dose_level_normalized_old" does not equal the new "dose_level_normalized"
  compare <- df_normalized %>% 
    # Round to 3 decimal places to help with comparison
    dplyr::mutate(dplyr::across(c(dose_level_normalized_old, dose_level_normalized_final), ~round(as.numeric(.), 3))) %>%
    dplyr::filter(xor(is.na(dose_level_normalized_old), is.na(dose_level_normalized_final))|dose_level_normalized_old != dose_level_normalized_final)
  
  # Units with conversions before, but not now
  message("Missing old logic unit conversions: ")
  compare %>% 
    dplyr::filter(!is.na(dose_level_normalized_old), is.na(dose_level_normalized_final)) %>%
    dplyr::pull(dose_level_units_old) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Units with conversions now, but not before
  message("Newly covered unit conversions: ")
  compare %>% 
    dplyr::filter(is.na(dose_level_normalized_old), !is.na(dose_level_normalized_final)) %>%
    dplyr::pull(dose_level_units_old) %>%
    unique() %>%
    paste0("- ", ., sep="\n") %>%
    cat()
  
  # Expand back out the ID field
  df_update <- df_normalized %>%
    tidyr::separate_rows(id, sep = ", ") %>%
    dplyr::distinct()
  
  if(nrow(df_update) != nrow(df_raw)){
    message("Error with normalization, input rows do not equal output...")
    browser()
    stop("Error with normalization, input rows do not equal output...")
  }
  ##############################################################################
  ### Check for updates to dose_target, level, and normalized field pairs
  ##############################################################################
  # List to store output
  df_out_list = list(normalized_full=df_update)
  # List of num-unit field pairs to check
  dose_list = list(
    normalized = c("dose_level_units_normalized", "dose_level_normalized"),
    target = c("dose_level_target_units", "dose_level_target"),
    dose_level = c("dose_level_units", "dose_level")
  )
  
  # Loop through and dynamically select the num-unit field pairs as needed from dose_list
  for(dose_n in names(dose_list)){
    d_unit = dose_list[[dose_n]][1]
    d_num = dose_list[[dose_n]][2]
    
    # Compare normalized units
    compare_norm_units = df_update %>%
      dplyr::mutate(compare = dplyr::case_when(
        is.na(!!as.name(paste0(d_unit, "_old"))) & is.na(!!as.name(paste0(d_unit, "_final"))) ~ TRUE,
        !is.na(!!as.name(paste0(d_unit, "_old"))) & is.na(!!as.name(paste0(d_unit, "_final"))) ~ FALSE,
        is.na(!!as.name(paste0(d_unit, "_old"))) & !is.na(!!as.name(paste0(d_unit, "_final"))) ~ FALSE,
        TRUE ~ !!as.name(paste0(d_unit, "_old")) == !!as.name(paste0(d_unit, "_final"))
      )) %>%
      dplyr::filter(compare == FALSE) %>%
      dplyr::select(dplyr::all_of(c("id", paste0(d_unit, "_final")))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(qc_notes = paste0(dose_n, " dose units updated"))
    
    # Remove "_final" stem and rename to database field
    names(compare_norm_units) <- names(compare_norm_units) %>% 
      gsub("_final", "", .) %>%
      gsub("\\bdose_level_units\\b", "dose_level_units_original", .)
    
    # Filter to only entries that need updating
    df_out = df_update %>%
      dplyr::mutate("{paste0(d_num, '_final')}" := as.character(!!as.name(paste0(d_num, "_final"))),
                    # Can't compare NA values, so replace for now
                    dplyr::across(dplyr::all_of(c(paste0(d_num, "_old"), paste0(d_num, "_final"))),
                                  ~tidyr::replace_na(., "-99999"))) %>%
      # tidyr::replace_na(list(!!as.name(paste0(d_num, "_old")) = "-99999", 
      #                        !!as.name(paste0(d_num, "_final")) = "-99999")) %>%
      dplyr::filter(!!as.name(paste0(d_num, "_old")) != !!as.name(paste0(d_num, "_final"))) %>%
      dplyr::filter(!!as.name(paste0(d_num, "_final")) != "-99999") %>%
      dplyr::select(dplyr::all_of(c("id", paste0(d_num, "_final")))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(qc_notes = paste0(dose_n, " dose updated"))
    
    # Remove "_final" stem and rename to database field
    names(df_out) <- names(df_out) %>%
      gsub("_final", "", .) %>%
      gsub("\\bdose_level\\b", "dose_level_original", .)
    
    # Only push updates if report.only is FALSE
    if(!report.only){
      if(nrow(df_out)){
        # Push updated values to Studies sheet based on "id" field
        db_update_tbl(df=df_out,
                      tblName = "studies")
      } else {
        message("No dose ", dose_n, " updates to push...")
      }
      
      if(nrow(compare_norm_units)){
        # Push updated conc_units_normalized
        db_update_tbl(df = compare_norm_units,
                      tblName = "studies")
      } else {
        message("No dose ", dose_n, " unit updates to push")
      }  
    } else {
      # Append to list to return
      df_out_list = append(df_out_list, 
                           list(df_out, 
                                compare_norm_units) %T>% { names(.) <- c(dose_n, paste0(dose_n, "_units")) })
    }
  }
  
  # Return list of df_out to review
  return(df_out_list)
}
