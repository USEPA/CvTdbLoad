# There are 4 situations
# 1. A record fails
#       - Remove these
# 2. A record passes with no modifications
#       - Do nothing
# 3. A record passes with modifications
#       - Update these entries in the database
# 4. A new record is added
#       - Add this new record to the database

# Conditionals are built to safely handle quality control issues
#   - For example, someone might put "PASS" on a new record which should not have a qc_status.
#     This could produce duplicate columns if conditionals are not carefully chosen

validate_qc_fields <- function(df) {
    validation <- TRUE # False if an invalid condition was encountered
    rules <- validate::validator(.file=paste0("input/rules/QC.yaml"))
    
    # Loop through each sheet
    for (sheet in names(df)) {
        # Pull rules from the respective YAML, and store failing validation checks
        out <- validate::confront(df[[sheet]], rules)
        fails <- validate::summary(out) %>% 
            dplyr::filter(fails > 0)

        # Loop through each failure and log the message
        for (i in seq_len(nrow(fails))) {
            validation = FALSE
            m <- validate::meta(rules[fails$name[i]])$message
            message(m)
            # log_CvT_doc_load(f=f, m=m, log_path=log_path)
        }
    }

    return(validation)
}

qc_to_cvt <- function(f) {
    doc_sheet_list <- load_sheet_group(fileName = f, template_path = "input/qc_template.xlsx")
    field_mapping <- readxl::read_xlsx("input/qa_template_map.xlsx")

    # Convert all qc_status and qc_flags to lowercase, for simpler comparison
    for (sheet in names(doc_sheet_list)) {
        doc_sheet_list[[sheet]] <- doc_sheet_list[[sheet]] %>%
            dplyr::mutate(
                qc_status = tolower(qc_status),
                qc_flags = tolower(qc_flags)
            )
    }

    if (isFALSE(validate_qc_fields(doc_sheet_list))) {
        message("Validation failed, exiting.")
        quit()
    }

    for (sheet in names(doc_sheet_list)) {
        # Get the sheet mapping to remap the fields
        sheet_mapping <- field_mapping %>%
            dplyr::filter(sheet == tolower(sheet_name))
    
        # Iterate over each row in the sheet_mapping and update the column name
        for (i in 1:nrow(sheet_mapping)) {
            to_column <- sheet_mapping$to[i]
            from_column <- sheet_mapping$from[i]
            
            test[[sheet_name]] <- test[[sheet_name]] %>%
                rename(!!from_column := !!sym(to_column))
        }

        categorized_records <- doc_sheet_list[[sheet]] %>%
            dplyr::mutate(
                category = case_when(
                    qc_status == "fail" ~ "Remove",
                    qc_flags == "modified" ~ "Update",
                    qc_flags == "new entry" | stringr::str_detect(qc_flags, "split entry") ~ "Add",
                    TRUE ~ "Ignore"
                )
            ) %>%
            dplyr::select(id, category)

        # TODO: Remove these ids from the database matching sheet to db table
        ids_to_remove <- dplyr::filter(categorized_records, category == "Remove")
        # query <- "DELETE FROM cvt.{sheet} WHERE id = {id}"

        # TODO: Map these to their database columns, while normalizing relevant fields
        # Update any fields that have changed
        ids_to_update <- dplyr::filter(categorized_records, category == "Update")
        # query <- "UPDATE cvt.{sheet} SET record = new_df[record][ids_to_update]"

        # TODO: Map these to their database columns, while normalizing relevant fields
        # Add these new rows to the database
        ids_to_add <- dplyr::filter(categorized_records, category == "Add")
        # query <- "INSERT INTO cvt.{sheet} (columns) VALUES records[ids_to_add]"
        
        # Likely shouldn't need to touch these passing, unmodified records
        ids_to_ignore <- dplyr::filter(categorized_records, category == "Ignore")

        # TODO: normalize_dose, normalize_time, normalize_conc, ideally only once
        # updated_sheet = doc_sheet_list[[sheet]] %>%
        #                     dplyr::mutate(
        #                        field_mapping[column] = column
        #                     )
    }
}

fileList <- c(
    "input/test_qc/PMID17668360_QC_template_bkesic_erowan_20240212.xlsx",
    "input/test_qc/test_validation.xlsx",
    "input/test_qc/test_invalidation.xlsx"
)
f <- fileList[[2]]

qc_to_cvt(f)
