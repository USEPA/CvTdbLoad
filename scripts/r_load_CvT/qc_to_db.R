map_to_database_fieldnames <- function(df) {
  field_mapping <- readxl::read_xlsx("input/qa_template_map.xlsx")
  
  for (sheet_name in names(df)) {
    # Get the sheet mapping to rename the qc template fields to db fields
    sheet_mapping <- field_mapping %>%
      dplyr::filter(sheet == tolower(sheet_name))
    
    # Iterate through each column for the respective sheet
    for (i in seq_len(nrow(sheet_mapping))) {
      new_value <- sheet_mapping$to[i]
      old_value <- sheet_mapping$from[i]
      
      # Rename the column if it exists in the sheet, otherwise skip it
      if (old_value %in% df[[sheet_name]]) {
        df[[sheet_name]] <- df[[sheet_name]] %>%
          dplyr::rename(!!old_value := !!new_value)
      }
    }
  }
  
  return (df)
}

qc_to_db <- function(files) {
  log_path <- "output/qc_to_db_log.xlsx"
  
  for (f in files) {
    doc_sheet_list <- load_sheet_group(fileName = f, template_path = "input/qc_template.xlsx")
    
    # Convert all qc_status and qc_flags to lowercase, for simpler comparison
    for (sheet_name in names(doc_sheet_list)) {
      doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
        dplyr::mutate(
          qc_status = tolower(qc_status),
          qc_flags = tolower(qc_flags)
        )
    }
    
    if (!validate_cvt(df=doc_sheet_list, log_path=log_path)) {
      message("Validation failed, exiting.")
      stop()
    }
    
    # Update the column names to match their database field names
    doc_sheet_list <- map_to_database_fieldnames(doc_sheet_list)
    
    # Set QC category to determine database action by status and flags
    doc_sheet_list = lapply(doc_sheet_list, function(sheet){
      sheet %>%
        # Categorize each record based on 4 conditions of remove, update, add, or ignore
        dplyr::mutate(
          category = dplyr::case_when(
            qc_status == "fail" ~ "Remove",
            qc_flags == "modified" ~ "Update",
            qc_flags == "new entry" | grepl("split entry", qc_flags) ~ "Add",
            TRUE ~ "Pass"
          )
        )
    }) %T>% {
      names(.) <- names(doc_sheet_list)
    }
    
    # Delete/remove records in specific order to handle cascade needs due to foreign key connections
    for(sheet_name in c("Conc_Time_Values", "Series", "Subjects", "Studies", "Documents")){
      # Remove these ids from the database
      qc_remove_record(df = doc_sheet_list[[sheet_name]] %>%
                         dplyr::filter(category == "Remove") %>%
                         dplyr::select(id, qc_notes, qc_flags),
                       tbl_name = sheet_name)
    }
    
    # Interate through each sheet and QC category to perform specific actions in the database
    for (sheet_name in names(doc_sheet_list)) {
      
      # Update unchanged records to qc_status = "pass"
      db_update_tbl(df = doc_sheet_list[[sheet_name]] %>%
                      dplyr::filter(category == "Pass") %>%
                      dplyr::select(id) %>%
                      dplyr::mutate(qc_status = "pass",
                                    qc_notes = "QC pass without changes"), 
                    tblName = sheet_name)
      
      # Add these new rows to the database
      new_records = qc_add_record(df = doc_sheet_list[[sheet_name]] %>%
                                    dplyr::filter(category == "Add"),
                                  tbl_name = sheet_name)
      
      # TODO Append updated ID values to doc_sheet_list for foreign key 
      # substitutions of QC_# values
      
      # TODO: Add check/select for only fields in database table
      # Update unchanged records to qc_status = "pass"
      db_update_tbl(df = doc_sheet_list[[sheet_name]] %>%
                      dplyr::filter(category == "Update") %>%
                      dplyr::mutate(qc_status = "pass"), 
                    tblName = sheet_name)
      
      # TODO: Run normalization of updated/added records
    }
  }
}