in_data = db_query_cvt(paste0("select distinct ",
                              "b.fk_extraction_document_id, ",
                              "b.fk_reference_document_id, ", 
                              "b.id as study_id, a.fk_subject_id, a.id as series_id ",
                              "from cvt.series a ",
                              "left join cvt.studies b ",
                              "on a.fk_study_id = b.id"))

# Collapse groups to get document and curation tag lists
shared_subs = in_data %>%
  dplyr::select(fk_extraction_document_id, fk_subject_id) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fk_subject_id) %>%
  # dplyr::summarise(n = n())
  dplyr::mutate(dplyr::across(dplyr::any_of(names(in_data)[!names(in_data) %in% c("fk_subject_id")]),
                                            ~paste0(sort(unique(.[!is.na(.)])),
                                                    collapse = ", ") %>%
                                              dplyr::na_if("NA") %>%
                                              dplyr::na_if("") %>%
                                              dplyr::na_if("-")
                                            )) %>%
  # dplyr::mutate(id = paste0(sort(unique(id[!is.na(id)])), collapse=", ")) %>%
  dplyr::distinct() %>%
  dplyr::filter(grepl(",", fk_extraction_document_id))
