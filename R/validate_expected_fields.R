#' @title Check expected fields
#' @description Function to check if processed document's column names appropriately match the blank template.
#' @param df List of dataframes for the sheets within an extraction template
#' @param f Filename for flagging purposes
#' @param log_path File path where to save the log file. Default "output/template_normalization_log.xlsx"
#' @param template_path File path to the location of a blank template for comparison
#' @return None. Logs any flags
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [validator][validate::validator]
#' @rdname validate_expected_fields
#' @export 
validate_expected_fields <- function(df, f, log_path, template_path, verbose=FALSE) {
  validation <- TRUE # False if an invalid condition was encountered
  
  if (any(is.na(df), is.na(f), is.na(log_path), is.na(template_path))) {
    stop("Must include a valid df, f, log_path, and template_path.")
  }

  # Loop through each sheet
  for (sheet in names(df)) {
    template_fields <- names(readxl::read_excel(f, sheet=sheet, .name_repair="minimal"))
    expected_fields <- names(readxl::read_excel(template_path, sheet=sheet))

    # Check that no field_names are duplicated
    if (any(duplicated(template_fields))) {
      m <- "Found duplicated field name(s)."
      message(paste0(sheet, ": ", m))
      log_CvT_doc_load(f=f, m=m, log_path=log_path)
      validation <- FALSE

      if (verbose) {
        cat(c(template_fields[duplicated(template_fields)], "\n"), sep=", ")
      }
    }

    # Check that all field names in the curated template exist in the blank template
    if (!all(template_fields %in% expected_fields)) {
      m <- "Found extraneous template field names."
      message(paste0(sheet, ": ", m))
      log_CvT_doc_load(f=f, m=m, log_path=log_path)
      validation <- FALSE       

      if (verbose) {
        cat(c(template_fields[!template_fields %in% expected_fields], "\n"), sep=", ")
      } 
    }
  }

  return (validation)
}