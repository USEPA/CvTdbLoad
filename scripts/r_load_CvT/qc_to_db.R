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

qc_to_cvt <- function(f) {
    qc_template <- "input/qc_template.xlsx"
    field_mapping <- "input/qa_template_map.xlsx"
    doc_sheet_list <- load_sheet_group(fileName = f, template_path = qc_template)

    for (sheet in names(doc_sheet_list)) {
        categorized_records <- doc_sheet_list[[sheet]] %>%
            dplyr::mutate(
                category = case_when(
                    qc_status == "FAIL" ~ "Remove",
                    qc_flags == "Modified" ~ "Update",
                    qc_flags == "New entry" | stringr::str_detect(qc_flags, "Split entry") ~ "Add",
                    TRUE ~ "Ignore"
                )
            ) %>%
            dplyr::select(id, category)

        # TODO: Remove these ids from the database matching sheet to db table
        ids_to_remove <- filter(categorized_records, category == "Remove")
        # query <- "DELETE FROM cvt.{sheet} WHERE id = {id}"

        # TODO: Map these to their database columns, while normalizing relevant fields
        # Update any fields that have changed
        ids_to_update <- filter(categorized_records, category == "Update")
        # query <- "UPDATE cvt.{sheet} SET record = new_df[record][ids_to_update]"

        # TODO: Map these to their database columns, while normalizing relevant fields
        # Add these new rows to the database
        ids_to_add <- filter(categorized_records, category == "Add")
        # query <- "INSERT INTO cvt.{sheet} (columns) VALUES records[ids_to_add]"
        
        # Likely shouldn't need to touch these passing, unmodified records
        ids_to_ignore <- filter(categorized_records, category == "Ignore")

        # TODO: normalize_dose, normalize_time, normalize_conc, ideally only once
        # updated_sheet = doc_sheet_list[[sheet]] %>%
        #                     dplyr::mutate(
        #                        field_mapping[column] = column
        #                     )
    }
}

fileList <- c(
    "input/test_qc/PMID17668360_QC_template_bkesic_erowan_20240212.xlsx"
)
f <- fileList[[1]]

qc_to_cvt(f)
