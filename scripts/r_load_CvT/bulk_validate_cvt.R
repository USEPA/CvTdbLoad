#' @title bulk_validate_cvt
#' @description Run validation for templates within an input directory
#' @param destination_folder Folder with templates to validate, Default: 'output/CVTDB QC'
#' @param log_path Filepath for log file to generate, Default: 'output/bulk_cvt_validation_log.xlsx'
#' @param template_path CvT template filepath, Default: 'input/CvT_data_template_articles.xlsx'
#' @return Log file is updated with error messages by template. Console messages are printed.
#' @rdname bulk_validate_cvt
#' @export
bulk_validate_cvt <- function(destination_folder = "output/CVTDB",
                              log_path = "output/bulk_cvt_validation_log.xlsx",
                              template_path = "input/CvT_data_template_articles.xlsx"){
  
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
  
}
