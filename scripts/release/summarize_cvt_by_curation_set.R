#' @title Get CvT Curation and QC Stats
#' @description Summarize CvT curation and QC overall and by tag Sets
#' @return None. XLSX files are written to output/release folder
get_cvt_curation_qc_stats <- function(){
  # Pull tags
  curation_sets = db_query_cvt("SELECT distinct curation_set_tag FROM cvt.documents") %>%
    dplyr::pull()
  # Replace NA with overall
  curation_sets[is.na(curation_sets)] <- "CvTdb Overall"
  
  # If not added, add it in so the overall summary is reported
  if(!"CvTdb Overall" %in% curation_sets) c(curation_sets, "CvTdb Overall")
  
  conc_medium = db_query_cvt(paste0("SELECT * FROM cvt.conc_medium_dict"))
  
  # Loop through each curation set and pull relevant stats
  curation_tag_summary = lapply(curation_sets, function(tag_n){
    message("Pulling ", tag_n)
    # Pull documents
    if(tag_n == "CvTdb Overall"){
      doc_query = paste0("select * from cvt.documents ",
                         "where id in (select fk_extraction_document_id from cvt.studies) ",
                         "or id in (select fk_reference_document_id from cvt.studies)")
    } else {
      doc_query = paste0("select id from cvt.documents ",
                         "where curation_set_tag = '", tag_n, "' and ",
                         "(id in (select fk_extraction_document_id from cvt.studies) ",
                         "or id in (select fk_reference_document_id from cvt.studies))")
    }
    docs = db_query_cvt(doc_query)
    if(!nrow(docs)) return(NULL)
    # Pull studies
    studies = db_query_cvt(paste0("SELECT * FROM cvt.studies WHERE fk_extraction_document_id in (", toString(docs$id), ") ",
                                  "OR fk_reference_document_id in (", toString(docs$id), ")"))
    # Pull series
    series = db_query_cvt(paste0("SELECT * FROM cvt.series WHERE fk_study_id in (", toString(unique(studies$id)), ")"))
    # Pull subjects
    subjects = db_query_cvt(paste0("SELECT * FROM cvt.subjects WHERE id in (", toString(unique(series$fk_subject_id)), ")"))
    # Pull conc_time_values
    conc = db_query_cvt(paste0("SELECT * FROM cvt.conc_time_values WHERE fk_series_id in (", toString(unique(series$id)), ")"))
    # Pull chemicals
    chems = db_query_cvt(paste0("SELECT * FROM cvt.chemicals WHERE id in (", 
                                toString(c(unique(studies$fk_dosed_chemical_id[!is.na(studies$fk_dosed_chemical_id)]),
                                           unique(series$fk_test_chemical_id[!is.na(series$fk_test_chemical_id)]),
                                           unique(series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)]))), 
                                ")"))
    
    data.frame(
      curation_set = tag_n,
      # Document Count
      n_total_documents = length(docs$id),
      n_extraction_documents = length(unique(studies$fk_extraction_document_id[!is.na(studies$fk_extraction_document_id)])),
      n_reference_documents = length(unique(studies$fk_reference_document_id[!is.na(studies$fk_reference_document_id)])),
      # Study Count
      n_studies = nrow(studies),
      # Subject Count
      n_subjects = nrow(subjects),
      # Series Count
      n_series = nrow(series),
      # Conc Media Count
      n_conc_time_values = nrow(conc),
      # Test Substance Count
      n_test_substance = length(unique(studies$fk_dosed_chemical_id[!is.na(studies$fk_dosed_chemical_id)])),
      n_test_substance_dtxsid = chems %>%
        dplyr::filter(id %in% studies$fk_dosed_chemical_id[!is.na(studies$fk_dosed_chemical_id)]) %>%
        pull(dsstox_substance_id) %>% unique() %>% length(),
      # Analyte Count
      n_analyte_substance = length(unique(series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)])),
      n_analyte_substance_dtxsid = chems %>%
        dplyr::filter(id %in% series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)]) %>%
        pull(dsstox_substance_id) %>% unique() %>% length(),
      # Conc Time Data Point Count
      n_conc_medium = length(unique(series$fk_conc_medium_id[!is.na(series$fk_conc_medium_id)])),
      n_conc_medium_normalized = conc_medium %>%
        dplyr::filter(id %in% unique(series$fk_conc_medium_id[!is.na(series$fk_conc_medium_id)])) %>%
        dplyr::pull(conc_medium_normalized) %>% unique() %>% length()
    ) %>% 
      return()
  }) %>%
    dplyr::bind_rows() %T>% 
    View("CvT Summary")
  
  writexl::write_xlsx(curation_tag_summary, paste0("output/release/cvtdb_stats_", Sys.Date(), ".xlsx"))
  ################################################################################
  ################################################################################
  ################################################################################
  # Pull QC tags
  curation_sets = db_query_cvt("SELECT distinct qc_set_tag FROM cvt.documents") %>%
    dplyr::pull() %>%
    .[!is.na(.)]
  
  conc_medium = db_query_cvt(paste0("SELECT * FROM cvt.conc_medium_dict"))
  
  # Loop through each curation set and pull relevant stats
  qc_tag_summary = lapply(curation_sets, function(tag_n){
    message("Pulling ", tag_n)
    # Pull documents
    doc_query = paste0("select id from cvt.documents ",
                       "where qc_set_tag = '", tag_n, "' and ",
                       "(id in (select fk_extraction_document_id from cvt.studies) ",
                       "or id in (select fk_reference_document_id from cvt.studies))")
    
    docs = db_query_cvt(doc_query)
    if(!nrow(docs)) return(NULL)
    # Pull studies
    studies = db_query_cvt(paste0("SELECT * FROM cvt.studies WHERE fk_extraction_document_id in (", toString(docs$id), ") ",
                                  "OR fk_reference_document_id in (", toString(docs$id), ")"))
    # Pull series
    series = db_query_cvt(paste0("SELECT * FROM cvt.series WHERE fk_study_id in (", toString(unique(studies$id)), ")"))
    # Pull subjects
    subjects = db_query_cvt(paste0("SELECT * FROM cvt.subjects WHERE id in (", toString(unique(series$fk_subject_id)), ")"))
    # Pull conc_time_values
    conc = db_query_cvt(paste0("SELECT * FROM cvt.conc_time_values WHERE fk_series_id in (", toString(unique(series$id)), ")"))
    # Pull chemicals
    chems = db_query_cvt(paste0("SELECT * FROM cvt.chemicals WHERE id in (", 
                                toString(c(unique(studies$fk_dosed_chemical_id[!is.na(studies$fk_dosed_chemical_id)]),
                                           unique(series$fk_test_chemical_id[!is.na(series$fk_test_chemical_id)]),
                                           unique(series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)]))), 
                                ")"))
    
    data.frame(
      curation_set = tag_n,
      # Document Count
      n_total_documents = length(docs$id),
      n_extraction_documents = length(unique(studies$fk_extraction_document_id[!is.na(studies$fk_extraction_document_id)])),
      n_reference_documents = length(unique(studies$fk_reference_document_id[!is.na(studies$fk_reference_document_id)])),
      # Study Count
      n_studies = nrow(studies),
      # Subject Count
      n_subjects = nrow(subjects),
      # Series Count
      n_series = nrow(series),
      # Conc Media Count
      n_conc_time_values = nrow(conc),
      # Test Substance Count
      n_test_substance = length(unique(studies$fk_dosed_chemical_id[!is.na(studies$fk_dosed_chemical_id)])),
      n_test_substance_dtxsid = chems %>%
        dplyr::filter(id %in% studies$fk_dosed_chemical_id[!is.na(studies$fk_dosed_chemical_id)]) %>%
        pull(dsstox_substance_id) %>% unique() %>% length(),
      # Analyte Count
      n_analyte_substance = length(unique(series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)])),
      n_analyte_substance_dtxsid = chems %>%
        dplyr::filter(id %in% series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)]) %>%
        pull(dsstox_substance_id) %>% unique() %>% length(),
      # Conc Time Data Point Count
      n_conc_medium = length(unique(series$fk_conc_medium_id[!is.na(series$fk_conc_medium_id)])),
      n_conc_medium_normalized = conc_medium %>%
        dplyr::filter(id %in% unique(series$fk_conc_medium_id[!is.na(series$fk_conc_medium_id)])) %>%
        dplyr::pull(conc_medium_normalized) %>% unique() %>% length()
    ) %>% 
      return()
  }) %>%
    dplyr::bind_rows() %T>% 
    View("CvT Summary")
  
  writexl::write_xlsx(qc_tag_summary, paste0("output/release/cvtdb_qc_stats_", Sys.Date(), ".xlsx"))
}
