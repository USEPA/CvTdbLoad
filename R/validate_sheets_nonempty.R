validate_sheets_nonempty <- function(df, f, log_path) {
    validation <- TRUE # False if an invalid condition was encountered

    sheetList <- c("Documents", "Studies", "Subjects", "Series", "Conc_Time_Values")
    #Check if file contains any blank sheets
    if(purrr::is_empty(df)){
        message("...Template has blank sheet: ", paste0(sheetList[!sheetList %in% names(df)], collapse = ", "))
        log_CvT_doc_load(f, m="blank_sheets", log_path=log_path)
        validation <- FALSE
    }

    return (validation)
}