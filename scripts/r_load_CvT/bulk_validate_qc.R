destination_folder <- "output/CVTDB QC"
log_path <- "output/bulk_qc_validation_log.xlsx"
labels <- list.files(destination_folder, full.names = TRUE)
failed <- list()
ready <- list()

for (label in labels) {
  ticket_numbers <- list.files(label, full.names = TRUE)

  for (ticket_number in ticket_numbers) {
    files <- list.files(ticket_number, full.names = TRUE)
    for (file in files) {
        message(paste0("Validating ", file))
        
        doc_sheet_list <- load_sheet_group(fileName = file, template_path = "input/qc_template.xlsx")
        
        for (sheet_name in names(doc_sheet_list)) {
            doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
                dplyr::mutate(
                    qc_status = tolower(qc_status),
                    qc_flags = tolower(qc_flags)
                )
        }

        validate_all_sheets_present(df=doc_sheet_list, f=file, log_path=log_path)
        validate_qc_fields(df=doc_sheet_list, f=file, log_path=log_path)
        check_required_fields_validator(df=doc_sheet_list, f=file, log_path=log_path)
    }
  }
}