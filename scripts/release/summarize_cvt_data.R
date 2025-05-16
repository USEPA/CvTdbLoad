#' @title Summarize CvT Data
#' @description Summarize CvT data for data release.
#' @return Dataframe of CvT Data summaries
summarize_cvt_data <- function(){
  
  out = data.frame(
    `Extraction Documents linked to a Study` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.documents ",
                                                       "WHERE id in (SELECT fk_extraction_document_id FROM cvt.studies)")) %>%
      dplyr::pull(n),
    
    `Reference Documents linked to a Study` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.documents ",
                                                    "WHERE id in (SELECT fk_reference_document_id FROM cvt.studies)")) %>%
      dplyr::pull(n),
    
    `Unique Chemical DTXSID Entries` = db_query_cvt(paste0("SELECT count(distinct dsstox_substance_id) as n FROM cvt.chemicals ",
                                                        "WHERE dsstox_substance_id IS NOT NULL AND ",
                                                        "(id IN (SELECT fk_dosed_chemical_id FROM cvt.studies) OR ",
                                                        "id IN (SELECT fk_analyzed_chemical_id FROM cvt.series))")) %>%
      dplyr::pull(n),
    
    `Total Studies` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.studies ",
                                          "WHERE id in (SELECT fk_study_id FROM cvt.series)")) %>%
      dplyr::pull(n),
    
    `Total Dosed Chemicals Mapped to DTXSID` = db_query_cvt(paste0("SELECT count(distinct dsstox_substance_id) as n FROM cvt.chemicals ",
                                                                   "WHERE dsstox_substance_id IS NOT NULL AND id in (SELECT fk_dosed_chemical_id FROM cvt.studies ",
                                                                   "WHERE id IN (SELECT DISTINCT fk_study_id FROM cvt.series))")) %>%
      dplyr::pull(n),
    
    `Total Administration Routes by Study Count` = db_query_cvt(paste0(
      "SELECT distinct a.id, b.administration_route_normalized ",
      "FROM cvt.studies a ",
      "LEFT JOIN cvt.administration_route_dict b ON a.fk_administration_route_id = b.id ",
      "WHERE a.id in (SELECT distinct fk_study_id FROM cvt.series) ",
      "AND b.administration_route_normalized IS NOT NULL"
    )) %>%
      dplyr::count(administration_route_normalized) %>%
      dplyr::arrange(administration_route_normalized) %>%
      dplyr::mutate(administration_route_normalized = paste0("**", administration_route_normalized, "**")) %>%
      tidyr::unite(col = "administration_route_normalized_n", administration_route_normalized, n, sep = " (") %>%
      dplyr::mutate(administration_route_normalized_n = paste0(administration_route_normalized_n, ")")) %>%
      dplyr::pull(administration_route_normalized_n) %>%
      paste0(collapse = ", "),
    
    `Total Subjects` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.subjects ",
                                           "WHERE id in (SELECT fk_subject_id FROM cvt.series)")) %>%
      dplyr::pull(n),
    
    `Total Species by Study Count` = db_query_cvt(paste0(
      "SELECT distinct a.fk_study_id, b.species ",
      "FROM cvt.series a ",
      "LEFT JOIN cvt.subjects b ON a.fk_subject_id = b.id"
    )) %>%
      dplyr::count(species) %>%
      dplyr::arrange(species) %>%
      dplyr::mutate(species = paste0("**", species, "**")) %>%
      tidyr::unite(col = "species_n", species, n, sep = " (") %>%
      dplyr::mutate(species_n = paste0(species_n, ")")) %>%
      dplyr::pull(species_n) %>%
      paste0(collapse = ", "),
    
    `Total Series` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.series WHERE id IN ",
                                         "(SELECT distinct fk_series_id FROM cvt.conc_time_values) ",
                                         "AND fk_study_id IN (SELECT id FROM cvt.studies WHERE fk_extraction_document_id IN (",
                                         "SELECT id FROM cvt.documents))")) %>%
      dplyr::pull(n),
    
    `Total Analyzed Chemicals Mapped to DTXSID` = db_query_cvt(paste0("SELECT count(distinct dsstox_substance_id) as n FROM cvt.chemicals ",
                                                                      "WHERE dsstox_substance_id IS NOT NULL AND id in (SELECT fk_analyzed_chemical_id FROM cvt.series)")) %>%
      dplyr::pull(n),
    
    `Total Studies Where the Dosed Chemical is Different from the Analyzed Chemical` = db_query_cvt(
      paste0(
        "SELECT ",
        # "b.id as series_id, b.fk_study_id, ",
        " DISTINCT b.fk_study_id ",
        # # Studies table fields
        # "c.fk_dosed_chemical_id, ",
        # ## Chemical dictionary fields (dosed chemical information)
        # "k.dosed_chem_dtxsid, ",
        # # Series table fields
        # "b.fk_analyzed_chemical_id, ",
        # 
        # ## Chemical dictionary fields (analyzed chemical information)
        # "l.analyzed_chem_dtxsid ",
        
        # Join with series table by series ID
        "FROM cvt.series b ",
        
        # Join to studies table by study ID
        "LEFT JOIN cvt.studies c ON b.fk_study_id = c.id ",
        
        # Rename chemical fields for dosed vs. analyzed chemical record foreign keys
        "LEFT JOIN (SELECT id, dsstox_substance_id as dosed_chem_dtxsid, ",
        "chemical_name_original as dosed_chem_name_original, dsstox_casrn as dosed_chem_casrn, preferred_name as dosed_chem_name ",
        "FROM cvt.chemicals) as k ON c.fk_dosed_chemical_id = k.id ",
        "LEFT JOIN (SELECT id, dsstox_substance_id as analyzed_chem_dtxsid, ",
        "chemical_name_original as analyzed_chem_name_original, dsstox_casrn as analyzed_chem_casrn, preferred_name as analyzed_chem_name ",
        "FROM cvt.chemicals) as l ON b.fk_analyzed_chemical_id = l.id ",
        "WHERE k.dosed_chem_dtxsid != l.analyzed_chem_dtxsid"
      )
    ) %>%
      # TODO add summary "n" field based on some grouping of records
      nrow(),
    
    `Total Concentration-Time Values with Normalized Units` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.conc_time_values ",
                                                                                  "WHERE fk_series_id in (SELECT id FROM cvt.series) ",
                                                                                  "AND conc IS NOT NULL")) %>%
      dplyr::pull(n),
    
    `Total TK Parameters by Extraction Document Count` = db_query_cvt(paste0(
      "SELECT distinct a.parameter_name, b.fk_extraction_document_id  ",
      "FROM cvt.tk_parameters a ",
      "LEFT JOIN cvt.studies b ON a.fk_study_id = b.id ",
      "UNION ",
      "SELECT distinct a.parameter_name, c.fk_extraction_document_id  ",
      "FROM cvt.tk_parameters a ",
      "LEFT JOIN cvt.series b ON a.fk_series_id = b.id ",
      "LEFT JOIN cvt.studies c ON b.fk_study_id = c.id"
    )) %>%
      dplyr::filter(!is.na(fk_extraction_document_id)) %>%
      dplyr::distinct() %>%
      dplyr::count(parameter_name) %>%
      dplyr::arrange(parameter_name) %>%
      dplyr::mutate(parameter_name = paste0("**", parameter_name, "**")) %>%
      tidyr::unite(col = "parameter_name_n", parameter_name, n, sep = " (") %>%
      dplyr::mutate(parameter_name_n = paste0(parameter_name_n, ")")) %>%
      dplyr::pull(parameter_name_n) %>%
      paste0(collapse = ", ")
  ) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        values_transform = as.character,
                        names_to = "Summary Name") %>%
    dplyr::mutate(`Summary Name` = `Summary Name` %>%
                    gsub(".", " ", ., fixed = TRUE))
  
  return(out)
}