validate_cvt <- function(
    clowder_file_id = NULL,
    file_path = NULL,
    db_identifier = NULL,
    df = NULL,
    log_path="output/validation/validate_cvt_log.xlsx",
    ignore_present = FALSE,
    ignore_nonempty = FALSE,
    ignore_required = FALSE,
    ignore_qc = FALSE,
    ignore_field_types = FALSE,
    ignore_field_entries = FALSE
  ) {

  template_path <- "input/CvT_data_template_articles.xlsx"
  template_map <- "input/qa_template_map.xlsx"

  # Pull document either locally, from the database, from clowder, or from a template df
  if (!is.null(file_path)) {
    doc_sheet_list <- load_sheet_group(fileName = file_path, template_path = template_path)
    f <- file_path
  }
  else if (!is.null(clowder_file_id)) {
    doc_sheet_list <- load_file_from_api(url = paste0(baseurl,"/api/files/",clowder_file_id,"/blob"),
                                        headers = c(`X-API-Key` = apiKey),
                                        mode = "wb",
                                        file_type = "xlsx")
    f <- clowder_file_id
  }
  else if (!is.null(db_identifier)) {
    doc_sheet_list <- cvtdb_to_template(id=db_identifier, template_path=template_path, template_map=template_map)
    f <- db_identifier
  } 
  else if (!is.null(df)) {
    doc_sheet_list <- df
    f <- "list_name"
  }
  else {
    stop("Must include a valid parameter. Either a clowder_file_id, file_path, db_identifier, or df.")
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

  if (!ignore_required) {
    valid_required_fields <- validate_required_fields(df=doc_sheet_list, f=f, log_path=log_path)
  } else {
    valid_required_fields <- TRUE
  }

  if (!ignore_qc) {
    valid_qc_fields <- validate_qc_fields(df=doc_sheet_list, f=f, log_path=log_path)
  } else {
    valid_qc_fields <- TRUE
  }

  if (!ignore_field_types) {
    valid_field_types <- validate_field_types(df=doc_sheet_list, f=f, log_path=log_path)
  } else {
    valid_field_types <- TRUE
  }

  if (!ignore_field_entries) {
    valid_field_entries <- validate_field_entries(df=doc_sheet_list, f=f, log_path=log_path)
  } else {
    valid_field_entries <- TRUE
  }

  document_validity <- all(sheets_present, sheets_nonempty, valid_required_fields, valid_qc_fields, valid_field_types, valid_field_entries)
  
  message(paste0("Document validity: ", document_validity, ". Output location: ", log_path, "."))
  return (document_validity)
}