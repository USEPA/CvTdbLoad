#' @title validate_sheets_present
#' @description Validate if template contains expected sheets
#' @param df Input named list of dataframes.
#' @param f Filename for logging purposes.
#' @param log_path Path to log.
#' @return Boolean of whether the input template passed the validation.
#' @rdname validate_sheets_present
#' @export 
validate_sheets_present <- function(df, f, log_path) {
    validation <- TRUE # False if an invalid condition was encountered

    sheetList <- c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values")
    # Check if file has all sheets, or it's only a Documents sheet
    if(any(!sheetList %in% names(df)) && !(length(names(df)) == 1 && names(df) == c("Documents"))){
        print(names(df))
        message("...Template missing sheet: ", paste0(sheetList[!sheetList %in% names(df)], collapse = ", "))
        log_CvT_doc_load(f, m="missing_sheets", log_path=log_path)
        validation <- FALSE
    }

    return (validation)
}
