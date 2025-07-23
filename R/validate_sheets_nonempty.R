#' @title validate_sheets_nonempty
#' @description Function to check if processed template contains expected non-empty sheets.
#' @param df Input named list of dataframes.
#' @param f Filename for logging purposes.
#' @param log_path Path to log.
#' @return Boolean of whether the input template passed the validation.
#' @rdname validate_sheets_nonempty
#' @export 
validate_sheets_nonempty <- function(df, f, log_path) {
  validation <- TRUE # False if an invalid condition was encountered
  
  # Remove empty dataframes from list
  df = df[sapply(df, nrow) > 0]
  
  sheetList <- c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values")
  # Check if df contains all sheetList sheets, and that it's not only a Documents sheet
  if(length(names(df)) == 0 || !all(sheetList %in% names(df)) && !(length(names(df)) == 1 && names(df) == c("Documents"))){
    message("...Template has blank sheet: ", paste0(sheetList[!sheetList %in% names(df)], collapse = ", "))
    log_CvT_doc_load(f, m="blank_sheets", log_path=log_path)
    validation <- FALSE
  }
  
  return (validation)
}
