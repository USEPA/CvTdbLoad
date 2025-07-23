#' @title validate_expected_fields
#' @description Function to check if processed document's column names appropriately match the blank template.
#' @param df Input named list of dataframes.
#' @param f Filename for logging purposes.
#' @param log_path Path to log.
#' @param template_path Path to blank template file for comparison.
#' @param verbose Boolean of whether to print additional console messages, Default: FALSE.
#' @return Boolean of whether the input template passed the validation.
#' @seealso 
#'  [validator][validate::validator]
#' @rdname validate_expected_fields
#' @export 
#' @importFrom readxl read_excel
validate_expected_fields <- function(df, f, log_path, template_path, verbose=FALSE) {
  validation <- TRUE # False if an invalid condition was encountered
  
  if (any(is.na(df), is.na(f), is.na(log_path), is.na(template_path))) {
    stop("Must include a valid df, f, log_path, and template_path.")
  }

  # Loop through each sheet
  for (sheet in names(df)) {
    template_fields <- names(df[[sheet]])
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
      m <- "Found duplicated or extraneous template field names."
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
