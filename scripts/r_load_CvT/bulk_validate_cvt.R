destination_folder <- "output/CVTDB QC"
log_path <- "output/bulk_cvt_validation_log.xlsx"
template_path <- "input/CvT_data_template_articles.xlsx"
labels <- list.files(destination_folder, full.names = TRUE)

for (label in labels) {
  ticket_numbers <- list.files(label, full.names = TRUE)
  
  for (ticket_number in ticket_numbers) {
    files <- list.files(ticket_number, full.names = TRUE)
    for (file in files) {
      message(paste0("Validating ", file))
      validate_cvt(file_path=f, log_path=log_path, ignore_qc=TRUE)
    }
  }
}