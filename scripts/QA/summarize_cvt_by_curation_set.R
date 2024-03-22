# Summarize CvT Overall and by Curation Sets

# Pull tags
curation_sets = db_query_cvt("SELECT distinct curation_set_tag FROM cvt.documents") %>%
  dplyr::pull()
# Replace NA with overall
curation_sets[is.na(curation_sets)] <- "overall"

# Loop through each curation set and pull relevant stats
out = lapply(curation_sets, function(tag_n){
  message("Pulling ", tag_n)
  # Pull documents
  if(tag_n == "overall"){
    doc_query = "SELECT * FROM cvt.documents"
  } else {
    doc_query = paste0("SELECT * FROM cvt.documents WHERE curation_set_tag = '", tag_n, "'")
  }
  docs = db_query_cvt(doc_query)
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
    n_total_documents = length(c(unique(studies$fk_reference_document_id),
                           unique(studies$fk_extraction_document_id))),
    n_extraction_documents = length(unique(studies$fk_extraction_document_id)),
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
    # Analyte Count
    n_analyte = length(unique(series$fk_analyzed_chemical_id[!is.na(series$fk_analyzed_chemical_id)])),
    # Conc Time Data Point Count
    n_conc_medium = length(unique(series$fk_conc_medium_id[!is.na(series$fk_conc_medium_id)]))
  ) %>% 
    return()
}) %>%
  dplyr::bind_rows() %T>% 
  View("CvT Summary")

################################################################################
### Summarize by Addition (What a Curation Set Added)
################################################################################