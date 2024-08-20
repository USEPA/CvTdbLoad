# Find document entries currently listed as having extracted data, but no study associations
in_data = db_query_cvt(paste0("select id, curation_set_tag, qc_set_tag, jira_ticket, clowder_template_id, qc_jira_ticket, qc_clowder_template_id, ",
                              "extracted, version, qc_status, qc_flags, qc_notes, created_by, rec_create_dt ",
                              "from cvt.documents ",
                              "WHERE id not in (SELECT fk_extraction_document_id ",
                              "FROM cvt.studies WHERE fk_extraction_document_id is not NULL) and ",
                              "extracted in ('1', '2', '3') and rec_create_dt > '2024-06-01'"))

# Pull study audit data to see if they ever had any
study_data = lapply(in_data$id, function(id){
  message(id)
  tmp = db_query_cvt(paste0('SELECT *',
                            'FROM cvt.cvt_audit WHERE record like \'%"fk_extraction_document_id" : "',id,'"%\'')) %>%
    dplyr::mutate(doc_id = !!id,
                  rec_create_dt = as.Date(rec_create_dt),
                  rec_end_dt = as.Date(rec_end_dt),
                  )
}) %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(fk_table_id) %>%
  dplyr::slice_max(version)

# Filter to document list
out = in_data %>%
  dplyr::filter(id %in% study_data$doc_id)
