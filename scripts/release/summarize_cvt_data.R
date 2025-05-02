#' @title Summarize CvT Data
#' @description Summarize CvT data for data release.
#' @return Dataframe of CvT Data summaries
summarize_cvt_data <- function(){
  
  out = data.frame(
    `Total Extraction Documents` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.documents ",
                                                       "WHERE id in (SELECT fk_extraction_document_id FROM cvt.studies)")) %>%
      dplyr::pull(n),
    `Total Unique DTXSID Entries` = db_query_cvt(paste0("SELECT count(distinct dsstox_substance_id) as n FROM cvt.chemicals ",
                                                        "WHERE dsstox_substance_id IS NOT NULL AND ",
                                                        "(id IN (SELECT fk_dosed_chemical_id FROM cvt.studies) OR ",
                                                        "id IN (SELECT fk_analyzed_chemical_id FROM cvt.series))")) %>%
      dplyr::pull(n),
    `Total Studies` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.studies ",
                                          "WHERE id in (SELECT fk_study_id FROM cvt.series)")) %>%
      dplyr::pull(n),
    `Total Dosed Chemicals` = db_query_cvt(paste0("SELECT count(distinct fk_dosed_chemical_id) as n FROM cvt.studies ",
                                                  "WHERE id in (SELECT fk_study_id FROM cvt.series)")) %>%
      dplyr::pull(n),
    `Total Dosed Chemicals Mapped to DTXSID` = db_query_cvt(paste0("SELECT count(distinct fk_dosed_chemical_id) as n FROM cvt.studies ",
                                                                   "WHERE id in (SELECT fk_study_id FROM cvt.series) AND ",
                                                                   "fk_dosed_chemical_id in (SELECT id FROM cvt.chemicals WHERE dsstox_substance_id IS NOT NULL)")) %>%
      dplyr::pull(n),
    `Total Subjects` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.subjects ",
                                           "WHERE id in (SELECT fk_subject_id FROM cvt.series)")) %>%
      dplyr::pull(n),
    `Species Breakdown` = db_query_cvt(paste0("SELECT species, count(*) as n FROM cvt.subjects ",
                                              "WHERE id in (SELECT fk_subject_id FROM cvt.series) ",
                                              "GROUP BY species")) %>%
      tidyr::unite(col = "species_n", species, n, sep = " (") %>%
      dplyr::mutate(species_n = paste0(species_n, ")")) %>%
      dplyr::pull(species_n) %>%
      paste0(collapse = ", "),
    `Total Series` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.series WHERE id IN ",
                                         "(SELECT distinct fk_series_id FROM cvt.conc_time_values) ",
                                         "AND fk_study_id IN (SELECT id FROM cvt.studies WHERE fk_extraction_document_id IN (",
                                         "SELECT id FROM cvt.documents))")) %>%
      dplyr::pull(n),
    `Total Analyzed Chemicals` = db_query_cvt(paste0("SELECT count(distinct fk_analyzed_chemical_id) as n FROM cvt.series ",
                                                     "WHERE id in (SELECT fk_series_id FROM cvt.conc_time_values)")) %>%
      dplyr::pull(n),
    `Total Analyzed Chemicals Mapped to DTXSID` = db_query_cvt(paste0("SELECT count(distinct fk_analyzed_chemical_id) as n FROM cvt.series ",
                                                                      "WHERE id in (SELECT fk_series_id FROM cvt.conc_time_values) AND ",
                                                                      "fk_analyzed_chemical_id in (SELECT id FROM cvt.chemicals WHERE dsstox_substance_id IS NOT NULL)")) %>%
      dplyr::pull(n),
    `Total Concentration-Time Values` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.conc_time_values ",
                                                            "WHERE fk_series_id in (SELECT id FROM cvt.series WHERE ",
                                                            "fk_study_id IN (SELECT id FROM cvt.studies WHERE fk_extraction_document_id IN (",
                                                            "SELECT id FROM cvt.documents))",
                                                            ")")) %>%
      dplyr::pull(n),
    `Total Concentration-Time Values with Normalized Units` = db_query_cvt(paste0("SELECT count(*) as n FROM cvt.conc_time_values ",
                                                                                  "WHERE fk_series_id in (SELECT id FROM cvt.series) ",
                                                                                  "AND conc IS NOT NULL")) %>%
      dplyr::pull(n)
  ) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        values_transform = as.character,
                        names_to = "Summary Name") %>%
    dplyr::mutate(`Summary Name` = `Summary Name` %>%
                    gsub(".", " ", ., fixed = TRUE))
  
  return(out)
}