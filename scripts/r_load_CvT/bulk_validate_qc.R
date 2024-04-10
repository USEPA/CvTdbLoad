destination_folder <- "output/CVTDB QC"
labels <- list.files(destination_folder, full.names = TRUE)
failed <- list()
ready <- list()

for (label in labels) {
  ticket_numbers <- list.files(label, full.names = TRUE)

  for (ticket_number in ticket_numbers) {
    files <- list.files(ticket_number, full.names = TRUE)
    for (file in files) {
        doc_sheet_list <- load_sheet_group(fileName = file, template_path = "input/qc_template.xlsx")
        for (sheet_name in names(doc_sheet_list)) {
            doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
                dplyr::mutate(
                    qc_status = tolower(qc_status),
                    qc_flags = tolower(qc_flags)
                )
        }
        
        validation <- validate_qc_fields(doc_sheet_list)
        if (!validation) {
            failed[[length(failed)+1]] = file
        } else {
            ready[[length(ready)+1]] = file
        }
    }
  }
}