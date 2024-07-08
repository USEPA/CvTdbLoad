map_to_database_fieldnames <- function(df) {
    field_mapping <- readxl::read_xlsx("input/qa_template_map.xlsx")
    
    for (sheet_name in names(df)) {
        # Get the sheet mapping to rename the qc template fields to db fields
        sheet_mapping <- field_mapping %>%
            dplyr::filter(sheet == tolower(sheet_name))

        # Iterate through each column for the respective sheet
        for (i in 1:nrow(sheet_mapping)) {
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

        # Interate through the sheets, and update each row to the database
        for (sheet_name in names(doc_sheet_list)) {
            # Categorize each record based on 4 conditions of remove, update, add, or ignore
            categorized_records <- doc_sheet_list[[sheet_name]] %>%
                dplyr::mutate(
                    category = dplyr::case_when(
                        qc_status == "fail" ~ "Remove",
                        qc_flags == "modified" ~ "Update",
                        qc_flags == "new entry" | stringr::str_detect(qc_flags, "split entry") ~ "Add",
                        TRUE ~ "Ignore"
                    )
                ) %>%
                dplyr::select(id, category)

            # Remove these ids from the database
            ids_to_remove <- dplyr::filter(categorized_records, category == "Remove" ) %>%
                dplyr::select(id)
            # query <- "DELETE FROM cvt.{sheet} WHERE id = {id}"
            
            # Likely shouldn't need to touch these passing, unmodified records
            ids_to_ignore <- dplyr::filter(categorized_records, category == "Ignore") %>%
                dplyr::select(id)

            # TODO: Normalize the rest of these fields
            # Update any fields that have changed
            ids_to_update <- dplyr::filter(categorized_records, category == "Update") %>%
                dplyr::select(id)
            # query <- "UPDATE cvt.{sheet} SET record = new_df[record][ids_to_update]"

            # Add these new rows to the database
            ids_to_add <- dplyr::filter(categorized_records, category == "Add") %>%
                dplyr::select(id)
            # query <- "INSERT INTO cvt.{sheet} (columns) VALUES records[ids_to_add]"

            print(paste(ids_to_add, ids_to_ignore, ids_to_remove, ids_to_update))
        }
    }
}