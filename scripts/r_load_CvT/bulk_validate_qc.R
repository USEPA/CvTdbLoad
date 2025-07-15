#' @title bulk_validate_qc
#' @description Run validation for QC templates within an input directory
#' @param destination_folder Folder with templates to validate, Default: 'output/CVTDB QC'
#' @param log_path Filepath for log file to generate, Default: 'output/bulk_cvt_validation_log.xlsx'
#' @param template_path CvT QC template filepath, Default: 'input/CvT_data_template_articles.xlsx'
#' @return Log file is updated with error messages by template. Console messages are printed.
#' @rdname bulk_validate_cvt
#' @export
bulk_validate_qc <- function(destination_folder = "output/CVTDB QC",
                             log_path = "output/bulk_qc_validation_log.xlsx",
                             template_path = "input/qc_template.xlsx"){
 
  labels <- list.files(destination_folder, full.names = TRUE)
  
  for (label in labels) {
    ticket_numbers <- list.files(label, full.names = TRUE)
    
    for (ticket_number in ticket_numbers) {
      files <- list.files(ticket_number, full.names = TRUE)
      for (file in files) {
        message(paste0("Validating ", file))
        
        doc_sheet_list <- load_sheet_group(fileName = file, template_path = template_path)
        
        for (sheet_name in names(doc_sheet_list)) {
          doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
            dplyr::mutate(
              qc_status = tolower(qc_status),
              qc_flags = tolower(qc_flags)
            )
        }
        
        validate_cvt(df=doc_sheet_list, log_path=log_path)
      }
    }
  }
   
}
