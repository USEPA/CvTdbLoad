# TODO: Add log path to preserve alignment with curation validation logic
validate_qc_fields <- function(df) {
    validation <- TRUE # False if an invalid condition was encountered
    rules <- validate::validator(.file=paste0("input/rules/QC.yaml"))
    
    # Loop through each sheet
    for (sheet_name in names(df)) {
        # Pull rules from the respective YAML, and store failing validation checks
        out <- validate::confront(df[[sheet_name]], rules)
        fails <- validate::summary(out) %>% 
            dplyr::filter(fails > 0)

        # Loop through each failure and log the message
        for (i in seq_len(nrow(fails))) {
            validation <- FALSE
            m <- validate::meta(rules[fails$name[i]])$message
            message(paste0(sheet_name, ": ", m))
            # log_CvT_doc_load(f=f, m=m, log_path=log_path)
        }
    }
    return (validation)
}

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
    curated_chemicals <- "input/chemicals/curated_chemicals_comparison_2021-11-23.xlsx"
    log_path <- "output/qc_to_db_log.xlsx"
    sheetList <- c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values")

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

        #Check if file contains all expected sheets
        if(any(!sheetList %in% names(doc_sheet_list))){
            message("...File missing sheet: ", paste0(sheetList[!sheetList %in% names(doc_sheet_list)], collapse = ", "), "...skipping...")
            log_CvT_doc_load(f, m="missing_sheets")
        }
        
        #Check if normalized data has all required fields (and no NA missing values in required fields)
        check_required_fields_validator(df=doc_sheet_list, f=f)

        # Validate that all qc_fields values are as expected
        if (!validate_qc_fields(doc_sheet_list)) {
            message("Validation failed, exiting.")
            stop()
        }

        #Normalize species
        doc_sheet_list$Subjects <- normalize_species(x=doc_sheet_list$Subjects)
        
        #Normalize administration route (use dictionary to map)
        doc_sheet_list$Studies <- doc_sheet_list$Studies %>%
            dplyr::rename(administration_route_original = administration_route) %>%
            mutate(administration_route_original = tolower(administration_route_original)) %>%
            left_join(readxl::read_xlsx("input\\dictionaries\\administration_route_dict.xlsx") %>%
                        dplyr::rename(fk_administration_route = id),
                        by="administration_route_original")
        
        #Check Species
        species_check <- db_query_cvt(paste0("SELECT DISTINCT species FROM cvt.subjects"))
        if(any(!doc_sheet_list$Subjects$species %in% species_check$species)){
            message("...File contains species not already in database: ", doc_sheet_list$Subjects$species[!doc_sheet_list$Subjects$species %in% species_check$species])
            log_CvT_doc_load(f, m="species_not_found")
        }
        
        #Match curated chemicals - rename columns to generic names 
        tmp <- chemical_curation_match_curated_chemicals(df=doc_sheet_list$Studies %>%
                                        select(name=test_substance_name, 
                                            name_secondary=test_substance_name_secondary, 
                                            casrn=test_substance_casrn), 
                                    f_name=curated_chemicals)
        doc_sheet_list$Studies = cbind(doc_sheet_list$Studies, tmp)
        
        #Match curated chemicals - rename columns to generic names 
        tmp <- chemical_curation_match_curated_chemicals(df=doc_sheet_list$Series %>%
                                        select(name=analyte_name, 
                                            name_secondary=analyte_name_secondary, 
                                            casrn=analyte_casrn), 
                                    f_name=curated_chemicals)
        doc_sheet_list$Series = cbind(doc_sheet_list$Series, tmp)
        
        #Call to the orchestration function for data normalization (with error logging)
        doc_sheet_list <- normalize_CvT_data(doc_sheet_list, f, log_path)
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
            ids_to_remove <- dplyr::filter(categorized_records, category == "Remove" )%>%
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