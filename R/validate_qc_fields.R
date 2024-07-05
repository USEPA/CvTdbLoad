validate_qc_fields <- function(df, f, log_path) {
    validation <- TRUE # False if an invalid condition was encountered
    rules <- validate::validator(.file=paste0("input/rules/qc/QC.yaml"))
    
    # Loop through each sheet
    for (sheet_name in names(df)) {
        # Check if not a QC document, or missing columns, or badly named columns
        if ((!"qc_status" %in% names(df[[sheet_name]])) || (!"qc_flags" %in% names(df[[sheet_name]]))) {
            return (FALSE)
        }
        
        # Convert all fields to lower
        df[[sheet_name]] <- df[[sheet_name]] %>%
            dplyr::mutate(
                qc_status = tolower(qc_status),
                qc_flags = tolower(qc_flags)
            )

        # Pull rules from the respective YAML, and store failing validation checks
        out <- validate::confront(df[[sheet_name]], rules)
        fails <- validate::summary(out) %>% 
            dplyr::filter(fails > 0)

        # Loop through each failure and log the message
        for (i in seq_len(nrow(fails))) {
            validation <- FALSE
            m <- validate::meta(rules[fails$name[i]])$message
            message(paste0(sheet, ": ", m))
            log_CvT_doc_load(f=f, m=m, log_path=log_path)
        }
    }

    return (validation)
}