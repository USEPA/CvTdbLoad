#' @title get.dictionary.entries.to.curate
#' @description Function to generate file of dictionary entries to curate
#' normalized field values. Includes foreign key identifiers to help query database
#' @param schema Database schema for PostgreSQL
#' @export
#' @return Dataframe list of dictionaries
get.dictionary.entries.to.curate <- function(schema, full.report=FALSE){
  
  # Map of dictionary tables to their foreign key tables/fields
  fk_list = list(
    administration_form_dict = c("studies", "fk_administration_form_id"),
    administration_method_dict  = c("studies", "fk_administration_method_id"),
    administration_route_dict  = c("studies", "fk_administration_route_id"),
    conc_medium_dict  = c("series", "fk_conc_medium_id"),
    dose_frequency_dict  = c("studies", "fk_dose_frequency_id")
  )
  
  # List of dictionary tables
  dict_list = db_query_cvt(paste0("SELECT table_name FROM information_schema.tables ",
                                  "WHERE table_schema = '", schema, "'",
                                  "AND table_name like '%_dict%'"
  ))
  
  # Loop through dict tables
  missing = lapply(dict_list$table_name, function(d_tbl){
    # Pull dictionary entries with a NULL normalized field
    in_data = db_query_cvt(paste0("SELECT * FROM ", schema, ".", d_tbl ,
                                  " WHERE ", gsub("_dict", "_normalized ", d_tbl), 
                                  "IS NULL"))
    
    # Pull associated record in foreign key table
    fk_tbl = fk_list[[d_tbl]]
    if(!full.report){
      fk_data = db_query_cvt(paste0("SELECT * FROM ", schema, ".", fk_tbl[1], 
                                    " WHERE ", fk_tbl[2], " IN (", toString(in_data$id), ")"))
    } else {
      fk_data = NULL
    }
    
    # Return as named list of dataframes
    list(in_data, fk_data) %T>% {
      names(.) <- c(d_tbl, paste0("fk_", d_tbl))
    } %>%
      return()
  }) %>%
    purrr::flatten() %>%
    purrr::compact()
  
  if(full.report){
    missing[["full_report"]] = paste0(
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
      # tk_parameters
      "m.*, m.id as tk_parameters_id ",
      
      # Join with series table by series ID
      "FROM cvt.series b ",
      
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
      "FROM cvt.chemicals) as l ON b.fk_analyzed_chemical_id = l.id ",
      # tk_parameters table join
      "LEFT JOIN cvt.tk_parameters m ON b.id = m.fk_series_id"
    ) %>% db_query_cvt() # %>%
      # # Remove extraneous fields 
      # dplyr::select(-dplyr::any_of(c(
      #   "analyte_name_original", "analyte_dtxsid", "analyte_casrn",
      #   "test_substance_dtxsid", "test_substance_casrn", "fk_test_chemical_id",
      #   "analyte_name_secondary_original", "analyte_casrn_original", "test_substance_name_original",
      #   "test_substance_name_secondary_original",
      #   "test_substance_casrn_original", "dose_frequency")))
  }
  
  # Export file and return dataframe list
  writexl::write_xlsx(missing, path = paste0("output/missing_dict_entries_", Sys.Date(), ".xlsx"))
  return(missing)
}