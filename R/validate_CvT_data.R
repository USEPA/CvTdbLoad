#' @title validate_cvt
#' @description Function to validate a CvTdb template based on a ruleset.
#' @param clowder_file_id Clowder template file identifier, Default: NULL
#' @param clowder_api_key Clowder API token, Default: NULL
#' @param file_path Path to template file, Default: NULL
#' @param db_identifier String or numeric identifier for a record in the database Documents table, Default: NULL
#' @param df Input named list of dataframes, Default: NULL
#' @param df_identifier String identifier for input `df`, required if `df` is not NULL, Default: NULL
#' @param log_path Path to log, Default: 'output/validation/validate_cvt_log.xlsx'
#' @param ignore_present Boolean of whether to ignore `validate_sheets_present()`, Default: FALSE
#' @param ignore_nonempty Boolean of whether to ignore `validate_sheets_nonempty()`, Default: FALSE
#' @param ignore_required Boolean of whether to ignore `validate_required_fields()`, Default: FALSE
#' @param ignore_qc Boolean of whether to ignore `validate_qc_fields()` and/or use the QC template for validation, Default: FALSE
#' @param ignore_field_types Boolean of whether to ignore `validate_field_types()`, Default: FALSE
#' @param ignore_field_entries Boolean of whether to ignore `validate_field_entries()`, Default: FALSE
#' @param ignore_field_uniqueness Boolean of whether to ignore `validate_field_uniqueness()`, Default: FALSE
#' @param ignore_foreign_keys Boolean of whether to ignore `validate_foreign_keys()`, Default: FALSE
#' @param ignore_expected_fields Boolean of whether to ignore `validate_expected_fields()`, Default: FALSE
#' @param verbose Boolean of whether to print additional console messages, Default: FALSE.
#' @return Boolean of whether the input template passed the validation.
#' @seealso 
#'  \code{\link[dplyr]{mutate}}
#' @rdname validate_CvT_data
#' @export 
#' @importFrom dplyr mutate
validate_cvt <- function(
    clowder_file_id = NULL,
    clowder_api_key=NULL,
    file_path = NULL,
    db_identifier = NULL,
    df = NULL,
    df_identifier = NULL,
    log_path="output/validation/validate_cvt_log.xlsx",
    ignore_present = FALSE,
    ignore_nonempty = FALSE,
    ignore_required = FALSE,
    ignore_qc = FALSE,
    ignore_field_types = FALSE,
    ignore_field_entries = FALSE,
    ignore_field_uniqueness = FALSE,
    ignore_foreign_keys = FALSE,
    ignore_expected_fields = FALSE,
    verbose=FALSE
  ) {

  cvt_template_path <- "input/CvT_data_template_articles.xlsx"
  qc_template_path <- "input/qc_template.xlsx"
  template_map <- "input/qa_template_map.xlsx"

  # Load in either the QC template or a standard template
  if (ignore_qc) {
    template_path <- cvt_template_path
  } else {
    template_path <- qc_template_path
  }

  # Pull document either locally, from the database, from clowder, or from a template df
  if (!is.null(file_path)) {
    doc_sheet_list <- load_sheet_group(fileName = file_path, template_path = template_path)
    f <- file_path
  } else if (!is.null(clowder_file_id)) {
    doc_sheet_list <- load_file_from_api(url = paste0("https://clowder.edap-cluster.com/api/files/",clowder_file_id,"/blob"),
                                        headers = c(`X-API-Key` = clowder_api_key),
                                        mode = "wb",
                                        file_type = "xlsx")
    f <- clowder_file_id
  } else if (!is.null(db_identifier)) {
    doc_sheet_list <- cvtdb_to_template(id=list(id=db_identifier), template_path=template_path, template_map=template_map)
    f <- db_identifier
  } else if (!is.null(df)) {
    if (is.null(df_identifier)) {
      stop("Must include parameter 'df_identifier' to uniquely identify the passed 'df'.")
    }
    doc_sheet_list <- df
    f <- df_identifier
  } else {
    stop("Must include a valid parameter. Either a clowder_file_id, file_path, db_identifier, or df.")
  }

  if (!ignore_qc) {
    # Convert qc_fields to lowercase for standardization
    for (sheet_name in names(doc_sheet_list)) {
      # Convert all fields to lower
      doc_sheet_list[[sheet_name]] <- doc_sheet_list[[sheet_name]] %>%
        dplyr::mutate(
          qc_status = tolower(qc_status),
          qc_flags = tolower(qc_flags)
        )
    }
  }
  
  # Run all desired validations
  if (!ignore_present) {
    sheets_present <- validate_sheets_present(df=doc_sheet_list, f=f, log_path=log_path)
  } else {
    sheets_present <- TRUE
  }

  if (!ignore_nonempty) {
    sheets_nonempty <- validate_sheets_nonempty(df=doc_sheet_list, f=f, log_path=log_path)
  } else {
    sheets_nonempty <- TRUE
  }

  if (!ignore_expected_fields) {
    valid_expected_fields <- validate_expected_fields(df=doc_sheet_list, f=f, log_path=log_path, template_path=template_path, verbose=verbose)
  } else {
    valid_expected_fields <- TRUE
  }

  if (!ignore_required) {
    valid_required_fields <- validate_required_fields(df=doc_sheet_list, f=f, log_path=log_path, verbose=verbose)
  } else {
    valid_required_fields <- TRUE
  }

  if (!ignore_qc) {
    valid_qc_fields <- validate_qc_fields(df=doc_sheet_list, f=f, log_path=log_path, verbose=verbose)
  } else {
    valid_qc_fields <- TRUE
  }

  if (!ignore_field_types) {
    valid_field_types <- validate_field_types(df=doc_sheet_list, f=f, log_path=log_path, verbose=verbose)
  } else {
    valid_field_types <- TRUE
  }

  if (!ignore_field_entries) {
    valid_field_entries <- validate_field_entries(df=doc_sheet_list, f=f, log_path=log_path, verbose=verbose)
  } else {
    valid_field_entries <- TRUE
  }

  if (!ignore_field_uniqueness) {
    valid_field_uniqueness <- validate_field_uniqueness(df=doc_sheet_list, f=f, log_path=log_path, verbose=verbose)
  } else {
    valid_field_uniqueness <- TRUE
  }

  if (!ignore_foreign_keys) {
    valid_foreign_keys <- validate_foreign_keys(df=doc_sheet_list, f=f, log_path=log_path, verbose=verbose)
  } else {
    valid_foreign_keys <- TRUE
  }

  document_validity <- all(sheets_present, sheets_nonempty, valid_expected_fields, valid_required_fields, valid_qc_fields, valid_field_types, valid_field_entries, valid_field_uniqueness, valid_foreign_keys)
  
  message(paste0("Document validity: ", document_validity, ". Output location: ", log_path, "."))
  return (document_validity)
}
