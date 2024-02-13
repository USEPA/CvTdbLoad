get_chems_for_curation <- function(){
  chem_curation_scripts = list.files("scripts/chemical_curation", full.names = TRUE)
  invisible(sapply(chem_curation_scripts, source,.GlobalEnv))
  
  in_data = db_query_cvt(paste0("SELECT id, chemical_name_original, ",
                                "chemical_name_secondary_original, ",
                                "casrn_original as casrn ",
                                "FROM cvt.chemicals WHERE dsstox_substance_id IS NULL")) %>%
    tidyr::pivot_longer(cols=c(-id, -casrn), values_to="name", names_to = "name_type") %>%
    tidyr::unite(id, name_type, col = "external_id", sep = "_") %>%
    tidyr::unite(name, casrn, col = "filter_key", sep = "_", remove=FALSE) %>%
    dplyr::filter(filter_key != "NA_NA") %>%
    dplyr::select(-filter_key)
  
  out = chem.check.v2(res0 = in_data)
  
  # Export raw and cleaned chems
  chems = in_data %>%
    dplyr::select(external_id, raw_name = name, raw_casrn = casrn) %>%
    dplyr::bind_cols(out$res0 %>%
                       dplyr::select(cleaned_name = name, cleaned_casrn = casrn)
                     ) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "NA"))) %T>% 
    writexl::write_xlsx(., paste0("output/chemical_mapping/cvtdb_chems_to_curate_",Sys.Date(),".xlsx"))
    

    return(chems)
}
