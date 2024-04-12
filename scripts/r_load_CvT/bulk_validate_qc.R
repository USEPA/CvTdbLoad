destination_folder <- "output/CVTDB QC"
log_path <- "output/qc_to_db_log.xlsx"
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
        
        print(file)

        #Check if file contains all expected sheets
        if(any(!sheetList %in% names(doc_sheet_list))){
            message("...File missing sheet: ", paste0(sheetList[!sheetList %in% names(doc_sheet_list)], collapse = ", "), "...skipping...")
            log_CvT_doc_load(f, m="missing_sheets")
        }

        validation <- validate_qc_fields(doc_sheet_list)
        # TODO: Need to update this to add to keep track of validation, or change other functions to log instead 
        check_required_fields_validator(df=doc_sheet_list, f=file, log_path=log_path)

        if (!validation | any(!sheetList %in% names(doc_sheet_list))) {
            failed[[length(failed)+1]] = file
        } else {
            ready[[length(ready)+1]] = file
        }
    }
  }
}