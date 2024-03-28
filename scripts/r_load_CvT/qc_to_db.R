# There are 4 situations
# 1. A record fails
#       - Remove these
# 2. A record passes with no modifications
#       - Do nothing
# 3. A record passes with modifications
#       - Update these entries in the database
# 4. A new record is added
#       - Add this new record to the database

qc_to_cvt <- function(f) {
    qc_template <- "input/qc_template.xlsx"
    field_mapping <- "input/qa_template_map.xlsx"
    doc_sheet_list <- load_sheet_group(fileName = f, template_path = qc_template)

    for (sheet in names(doc_sheet_list)) {
        # Handle failed records
        ids_to_remove <- doc_sheet_list[[sheet]] %>% 
            dplyr::filter(qc_status == "FAIL" || qc_flags == "Not Suitable" || qc_flags == "Split entry, original")  %>%
            dplyr::select(id)
        # TODO: Remove these ids from the database matching sheet to db table
        # "DELETE FROM cvt.{sheet} WHERE id = {id}"

        # All remaining records are assumed to pass (QC SOP)
        pass_records <- doc_sheet_list[[sheet]] %>% 
            dplyr::filter(qc_status != "FAIL" && qc_flags != "Not Suitable" && qc_flags != "Split entry, original")

        # Handle modified records
        ids_to_update <- pass_records %>%
            dplyr::filter(qc_flags == "Modified") %>% 
            dplyr::select(id)
        # TODO: Map these to their database columns, while normalizing relevant fields
        # Update any fields that have changed

        # Handle new records
        ids_to_add <- pass_records %>% 
            dplyr::filter(qc_flags == "New entry" || qc_flags == "Split entry") %>% 
            dplyr::select(id)
        # TODO: Map these to their database columns, while normalizing relevant fields
        # Add these new rows to the database
    }
}

fileList <- c(
    "input/test_qc/PMID17668360_QC_template_bkesic_erowan_20240212.xlsx"
)
f <- fileList[[1]]

qc_to_cvt(f)
