summarise_units_not_normalized <- function(){
  
  norm_script_list = list.files("scripts/bulk_update") %>%
    .[!grepl("unhandled|species", .)] %>%
    gsub("bulk_update_", "", .) %>%
    gsub("_normalization.R", "", ., fixed = TRUE)
  
  norm_queries = list(
    age = db_query_cvt(paste0("SELECT id as subject_id, species, age, age_units, age_category FROM cvt.subjects ",
                              "WHERE age_category is NULL and age is NOT NULL")),
    conc = db_query_cvt(paste0("SELECT distinct ",
                               "b.id as conc_time_values_id, a.conc_units_original, ",
                               "b.conc_original, b.conc, b.conc_sd_original, b.conc_lower_bound_original, b.conc_upper_bound_original, ",
                               "c.chemical_name_original, c.chemical_name_secondary_original, c.casrn_original, c.id as fk_analyzed_chemical_id, ",
                               "d.conc_medium_normalized, ",
                               "f.test_environment_temperature, a.radiolabeled, ",
                               "e.species ",
                               "FROM cvt.series a ",
                               "LEFT JOIN cvt.conc_time_values b ON a.id = b.fk_series_id ",
                               "LEFT JOIN cvt.chemicals c ON c.id = a.fk_analyzed_chemical_id ",
                               "LEFT JOIN cvt.conc_medium_dict d ON d.id = a.fk_conc_medium_id ",
                               "LEFT JOIN cvt.subjects e ON a.fk_subject_id = e.id ",
                               "LEFT JOIN cvt.studies f ON a.fk_study_id = f.id ",
                               "WHERE b.conc IS NULL and b.conc_original is NOT NULL AND b.conc_original NOT in ('ND', 'NA', 'NQ') ",
                               "AND d.conc_medium_normalized IS NOT NULL AND d.units IS NOT NULL"
    )),
    dose = db_query_cvt(paste0("SELECT b.id as study_id, a.fk_study_id as series_fk_study_id, ",
                               "e.chemical_name_original as test_substance_name, ",
                               "b.dose_level_original as dose_level, b.dose_level_units_original as dose_level_units, ",
                               "b.dose_level_target, b.dose_level_target_units, ",
                               "b.dose_volume, b.dose_volume_units, ",
                               "b.fk_dosed_chemical_id, b.dose_level_normalized, b.dose_level_units_normalized, ",
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
                               "WHERE b.dose_level_normalized is NULL and b.dose_level_original is NOT NULL ",
                               # Temporarily not normalizing dermal route doses - figuring out best units and conversions
                               "AND d.administration_route_normalized not in ('dermal')"
    )),
    height = db_query_cvt(paste0("SELECT id as subject_id, height, height_units, height_cm FROM cvt.subjects ",
                                 "WHERE height_cm is NULL and height is NOT NULL")),
    weight = db_query_cvt(paste0("SELECT id as subject_id, weight, weight_units, weight_kg FROM cvt.subjects ",
                                 "WHERE weight_kg is NULL and weight is NOT NULL AND weight not in ('NR', 'NA')")),
    time = db_query_cvt(paste0("SELECT a.id as conc_time_values_id, a.time_original, a.time_hr, b.time_units_original ",
                               "FROM cvt.conc_time_values a ",
                               "LEFT JOIN cvt.series b ON a.fk_series_id = b.id ",
                               "WHERE a.time_original is not NULL AND a.time_hr is NULL AND a.time_original NOT IN ('NA', 'NR')"))
  )
  
  # Check if any bulk normalization script queries are missing
  missing_script_query = norm_script_list[!norm_script_list %in% names(norm_queries)]
  
  if(length(missing_script_query)){
    stop("Missing script query for: ", toString(missing_script_query))
  }
  
  norm_counts = lapply(names(norm_queries), function(q_name){
    # Select columns to summarize by
    unit_name = switch(
      q_name,
      "age" = c("species", "age_units"),
      "conc" = c("conc_units_original"), 
      "dose" = "dose_level_units", 
      "height" = "height_units", 
      "weight" = "weight_units", 
      "time" = "time_units_original"
    )
    
    norm_queries[[q_name]] %>%
      dplyr::count(dplyr::across(dplyr::all_of(unit_name))) %>%
      return()
  }) %T>% {
    names(.) <- paste0(names(norm_queries), "_count")
  }
  
  # Create ID list, remove NA values
  id_list = norm_queries %>%
    dplyr::bind_rows() %>%
    dplyr::select(study_id, subject_id, conc_time_values_id) %>%
    dplyr::distinct() %>%
    as.list() %>%
    # Remove NA values in each ID vector
    purrr::modify(~ .[!is.na(.)])
  
  # Get full records for flagged records
  full_record = paste0(
    "SELECT distinct ",
    
    # Documents
    "e.*, e.id as document_id, ",
    # Studies
    "c.*, c.id as study_id, ",
    ## Chemical dictionary fields (dosed chemical information)
    "k.dosed_chem_dtxsid, k.dosed_chem_name_original, k.dosed_chem_casrn, k.dosed_chem_name, ",
    "j.dose_frequency_original, j.dose_frequency_normalized, ",
    ## administration_route dictionary fields
    "h.administration_route_original, h.administration_route_normalized, ",
    ## administration_method dictionary fields
    "g.administration_method_original, g.administration_method_normalized, ",
    ## administration_form dictionary fields
    "f.administration_form_original, f.administration_form_normalized, ",
    # Subjects
    "d.*, d.id as subject_id, ",
    # Series
    "b.*, b.id as series_id, ",
    ## Chemical dictionary fields (analyzed chemical information)
    "l.analyzed_chem_dtxsid, l.analyzed_chem_name_original, l.analyzed_chem_casrn, l.analyzed_chem_name, ",
    ## conc_medium dictionary fields
    "i.conc_medium_original, i.conc_medium_normalized, ", 
    # con_time_values
    "m.*, m.id as conc_time_values_id ",
    
    # Join with series table by series ID
    "FROM cvt.series b ",
    
    # conc_time_values table join
    "LEFT JOIN cvt.conc_time_values m ON b.id = m.fk_series_id ",
    
    # Join to studies table by study ID
    "LEFT JOIN cvt.studies c ON b.fk_study_id = c.id ",
    
    # Join to subjects table by subject ID
    "LEFT JOIN cvt.subjects d ON b.fk_subject_id = d.id ",
    
    # Join to documents table by extraction document ID
    "LEFT JOIN cvt.documents e ON c.fk_extraction_document_id = e.id ",
    
    # Join to dictionary tables
    "LEFT JOIN cvt.administration_form_dict f ON c.fk_administration_form_id = f.id ",
    "LEFT JOIN cvt.administration_method_dict g ON c.fk_administration_method_id = g.id ",
    "LEFT JOIN cvt.administration_route_dict h ON c.fk_administration_route_id = h.id ",
    "LEFT JOIN cvt.conc_medium_dict i ON b.fk_conc_medium_id = i.id ",
    "LEFT JOIN cvt.dose_frequency_dict j ON c.fk_dose_frequency_id = j.id ",
    
    # Rename chemical fields for dosed vs. analyzed chemical record foreign keys
    "LEFT JOIN (SELECT id, dsstox_substance_id as dosed_chem_dtxsid, ",
    "chemical_name_original as dosed_chem_name_original, dsstox_casrn as dosed_chem_casrn, preferred_name as dosed_chem_name ",
    "FROM cvt.chemicals) as k ON c.fk_dosed_chemical_id = k.id ",
    "LEFT JOIN (SELECT id, dsstox_substance_id as analyzed_chem_dtxsid, ",
    "chemical_name_original as analyzed_chem_name_original, dsstox_casrn as analyzed_chem_casrn, preferred_name as analyzed_chem_name ",
    "FROM cvt.chemicals) as l ON b.fk_analyzed_chemical_id = l.id "
  ) %>% db_query_cvt()
  
  # Rename duplicate columns as unique
  names(full_record) <- make.unique(names(full_record))
  # Remove duplicate "id" column names
  full_record = full_record %>%
    dplyr::select(-dplyr::starts_with("id"))
  
  # Pull studies without series
  control_studies = paste0("SELECT b.*, b.id as document_id, ",
                           "a.*, a.id as study_id ",
                           "FROM cvt.studies a ",
                           "LEFT JOIN cvt.documents b on a.fk_extraction_document_id = b.id ",
                           "WHERE a.id in (", toString(id_list$study_id[!id_list$study_id %in% full_record$study_id]), ")") %>%
    db_query_cvt()
  
  # Rename duplicate columns as unique
  names(control_studies) <- make.unique(names(control_studies))
  # Remove duplicate "id" column names
  control_studies = control_studies %>%
    dplyr::select(-dplyr::starts_with("id"))
  
  # Check if all relevant IDs were pulled
  if(any(
    any(id_list$study_id[!id_list$study_id %in% c(full_record$study_id, control_studies$study_id)]),
    any(id_list$subject_id[!id_list$subject_id %in% full_record$subject_id]),
    any(id_list$conc_time_values_id[!id_list$conc_time_values_id %in% full_record$conc_time_values_id])
  )){
    stop("Missing pulled id_list values...")
  }
  
  # Append and filter full records
  norm_counts$control_studies = control_studies
  norm_counts$norm_study_records = full_record %>%
    dplyr::filter(study_id %in% id_list$study_id)
  norm_counts$norm_subject_records = full_record %>%
    dplyr::filter(subject_id %in% id_list$subject_id)
  norm_counts$norm_conc_records = full_record %>%
    dplyr::filter(conc_time_values_id %in% id_list$conc_time_values_id)
    
  # Export
  writexl::write_xlsx(norm_counts, 
                      paste0("output/normalized_units_summary_log_", Sys.Date(), ".xlsx"))

}