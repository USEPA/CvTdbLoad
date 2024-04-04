# There are 4 situations
# 1. A record fails
#       - Remove these
# 2. A record passes with no modifications
#       - Do nothing
# 3. A record passes with modifications
#       - Update these entries in the database
# 4. A new record is added
#       - Add this new record to the database

# TODO: Maybe validate cvt fields alongside validating qc fields. So that we can rename all the columns, if we need all the columns. Currently just skipping non-existing columns, which might be a bad idea.
# TODO: Still need to handle normalization in this workflow

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
            validation = FALSE
            m <- validate::meta(rules[fails$name[i]])$message
            message(paste0(sheet_name, ": ", m))
            # log_CvT_doc_load(f=f, m=m, log_path=log_path)
        }
    }

    if (isFALSE(validation)) {
        message("Validation failed, exiting.")
        stop()
    }
}

qc_to_cvt <- function(f) {
    doc_sheet_list <- load_sheet_group(fileName = f, template_path = "input/qc_template.xlsx")
    field_mapping <- readxl::read_xlsx("input/qa_template_map.xlsx")

    # Convert all qc_status and qc_flags to lowercase, for simpler comparison
    for (sheet_name in names(doc_sheet_list)) {
        doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
            dplyr::mutate(
                qc_status = tolower(qc_status),
                qc_flags = tolower(qc_flags)
            )
    }

    # Validate that all qc_fields values are as expected
    validate_qc_fields(doc_sheet_list)

    for (sheet_name in names(doc_sheet_list)) {
        # Get the sheet mapping to remap the fields
        sheet_mapping <- field_mapping %>%
            dplyr::filter(sheet == tolower(sheet_name))

        # Iterate over each row in the sheet_mapping and update the column name
        for (i in 1:nrow(sheet_mapping)) {
            new_value <- sheet_mapping$to[i]
            old_value <- sheet_mapping$from[i]
            
            # Rename the column if it exists in the sheet, otherwise skip it
            if (old_value %in% doc_sheet_list[[sheet_name]]) {
                doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
                    dplyr::rename(!!old_value := !!new_value)
            }
        }

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
        ids_to_remove <- dplyr::filter(categorized_records, category == "Remove")
        # query <- "DELETE FROM cvt.{sheet} WHERE id = {id}"

        # Update any fields that have changed
        ids_to_update <- dplyr::filter(categorized_records, category == "Update")
        # query <- "UPDATE cvt.{sheet} SET record = new_df[record][ids_to_update]"

        # Add these new rows to the database
        ids_to_add <- dplyr::filter(categorized_records, category == "Add")
        # query <- "INSERT INTO cvt.{sheet} (columns) VALUES records[ids_to_add]"
        
        # Likely shouldn't need to touch these passing, unmodified records
        ids_to_ignore <- dplyr::filter(categorized_records, category == "Ignore")
    }
}

fileList <- c(
    "input/test_qc/PMID17668360_QC_template_bkesic_erowan_20240212.xlsx",
    "input/test_qc/test_validation.xlsx",
    "input/test_qc/test_invalidation.xlsx",
    "input/test_qc/PMID24495244_QC_template_bkesic_mhuse_20240206.xlsx"
)
f <- fileList[[1]]

qc_to_cvt(f)
