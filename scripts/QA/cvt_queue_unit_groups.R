cvt_queue_unit_groups <- function(in_unit_type = c("doses", "conc")){
  
  # Load dictionary of unit filenames
  norm_dir = readxl::read_xlsx("input/dictionaries/cvt_queue_unit_groups_norm_dir.xlsx")
  
  for(unit_type in in_unit_type){
    message("Working on unit: ", unit_type)
    dose_unit_groups = switch(
      unit_type,
      "doses" = list(
        unit_field = "dose_level_units_original",
        table = "Studies",
        in_data = db_query_cvt(paste0("SELECT distinct fk_extraction_document_id as doc_id, ",
                                      "dose_level_units_original FROM cvt.studies")) %>%
          dplyr::group_split(dose_level_units_original)
      ),
      "conc" = list(
        unit_field = "conc_units_original",
        table = "Conc_Time_Values",
        in_data = db_query_cvt(paste0("SELECT distinct b.fk_extraction_document_id as doc_id, ",
                                      "a.conc_units_original ",
                                      "FROM cvt.series a ",
                                      "LEFT JOIN cvt.studies b ",
                                      "ON a.fk_study_id = b.id ",
                                      "LEFT JOIN cvt.conc_medium_dict d ",
                                      "ON d.id = a.fk_conc_medium_id ",
                                      "WHERE a.id in (SELECT distinct fk_series_id ",
                                      "FROM cvt.conc_time_values WHERE conc_original IS NOT NULL) AND ",
                                      "d.conc_medium_normalized IS NOT NULL"
                                      )) %>%
          dplyr::group_split(conc_units_original)
      ),
      # Unhandled case = NULL
      NULL
    )
    
    # Check if input is a handled case
    if(is.null(unit_type)){
      stop("Unit type ", unit_type, " not a valid type...")
      return()
    }
    
    outputDir = paste0("output/Normalized Units QC/", unit_type)
    
    for(i in seq_len(length(dose_unit_groups$in_data))){
      unit_group = dose_unit_groups$in_data[[i]]
      unit_field = dose_unit_groups$unit_field
      unit_name = unique(unit_group[[unit_field]])
      
      message("Pulling group ", unit_name, " (", i, " of ", length(dose_unit_groups$in_data), ")")
      
      unit_file_name = norm_dir %>%
        dplyr::filter(unit_type == !!unit_type,
                      units == unit_name) %>%
        dplyr::pull(filename) %>%
        unique()
      
      if(length(unit_file_name) != 1){
        message(unit_name, " is missing a filename or has conflicting filenames")
        browser()
      }  
      # Create subfolder as needed
      unit_group_dir = file.path(outputDir, unit_file_name)
      
      if(!dir.exists(unit_group_dir)) dir.create(unit_group_dir, recursive = TRUE)
      
      # Check if files already exist
      group_f_list = list.files(unit_group_dir) %>%
        .[!grepl("log", .)]
      if(length(group_f_list)){
        group_f_list = group_f_list %>%
          gsub("doc_", "", .) %>%
          gsub("_unit_qc.xlsx", "", ., fixed = TRUE) %>%
          as.numeric()
      }
      
      # Filter out those already created
      unit_group = unit_group %>%
        dplyr::filter(!doc_id %in% group_f_list)
      
      # Skip if all have previously been exported
      if(!nrow(unit_group)) next
      
      id_list = list(id = unique(unit_group$doc_id))
      qc_templates = orchestrate_cvtdb_to_template(id_list = id_list, export = FALSE)
      
      # TODO Filter template list to records with dose units of the unit_name
      
      # Export Templates
      for(doc_id in names(qc_templates)){
        f_name = paste0(doc_id, "_unit_qc.xlsx")
        writexl::write_xlsx(qc_templates[[doc_id]],
                            file.path(unit_group_dir, f_name))
      }
      
      # Create checklist XLSX
      log = data.frame(filename = list.files(unit_group_dir)) %>%
        dplyr::mutate(
          table = dose_unit_groups$table, 
          field = dose_unit_groups$unit_field %>%
            gsub("_original", "", .),
          units = unit_name,
          normalized_units = NA,
          unit_conversion_method = NA,
          notes = NA
        )
      writexl::write_xlsx(log, file.path(unit_group_dir, 
                                         paste0(unit_file_name, "_unit_log.xlsx")))
    } 
    
    # Delete duplicate templates generated on the Document level
    qc_temp_list = list.files(outputDir, recursive = TRUE, full.names = TRUE) %>%
      data.frame(filepath = .) %>%
      dplyr::mutate(filename = basename(filepath)) %>%
      dplyr::filter(duplicated(filename))
    # Delete if there are duplicates
    if(nrow(qc_temp_list)){
      invisible(file.remove(qc_temp_list$filepath))  
    }
  }
}
