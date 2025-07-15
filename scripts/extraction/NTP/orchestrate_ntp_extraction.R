#' @title orchestrate_ntp_extraction
#' @description Function to use all NTP extraction helper functions to perform NTP extraction and export
#' @param f_list Input filepaths to NTP Excel files to extract. Must be full paths
#' @param skip_list List of NTP identifiers to skip due to autoextract issues, Default: c("S0640", "C96016", "C96019", "S0575", "S0636")
#' @return None. All outputs are stored in XLSX files in output/NTP directory.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  orchestrate_ntp_extraction(f_list=c("*path to NTP XLSX file.xlsx*"))
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{separate}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname orchestrate_ntp_extraction
#' @export 
#' @importFrom dplyr mutate
#' @importFrom tidyr separate
#' @importFrom writexl write_xlsx
orchestrate_ntp_extraction <- function(f_list, skip_list = c("S0640", "C96016", "C96019", 
                                                             "S0575", "S0636")){
  
  # Set up output directories
  if(!dir.exists("output")) dir.create("output")
  if(!dir.exists("output/NTP")) dir.create("output/NTP")
  if(!dir.exists("output/NTP/extraction")) dir.create("output/NTP/extraction")
  if(!dir.exists("output/NTP/qc")) dir.create("output/NTP/qc")
  if(!dir.exists("output/NTP/skipped")) dir.create("output/NTP/skipped")
  # Process input to get identifiers and output paths
  in_files = f_list %>%
    data.frame(files=.) %>%
    dplyr::mutate(other_study_identifier = basename(files),
                  out_file = files %>%
                    basename() %>%
                    gsub(".xlsx", "_autoextract.xlsx", .) %>%
                    paste0("output/NTP/extraction/", .)
                  ) %>%
    tidyr::separate(other_study_identifier, 
                    "other_study_identifier", 
                    sep="_", extra = "drop")
  # Determine which already exist in CvT
  in_cvt = db_query_cvt(paste0("SELECT other_study_identifier FROM cvt.documents WHERE other_study_identifier in ('",
                               paste0(in_files$other_study_identifier, collapse = "', '"),
                               "')")) %>% .[[1]]
  
  # Rename output for skipped files
  in_files$out_file[in_files$other_study_identifier %in% skip_list] = in_files$out_file[in_files$other_study_identifier %in% skip_list] %>%
    gsub("/extraction/", "/skipped/", ., fixed=TRUE)
  # Rename output for files already in CvT
  in_files$out_file[in_files$other_study_identifier %in% in_cvt] = in_files$out_file[in_files$other_study_identifier %in% in_cvt] %>%
    gsub("/extraction/", "/qc/", ., fixed=TRUE)
  
  # Loop through each file, extract, and export
  for(r in seq_len(nrow(in_files))){
    # Progress message every 25
    if(r %% 25 == 0){
      message("Processed ", r, " of ", nrow(in_files))
    }
    # Store in/out file name and other_study_identifier
    f = in_files$files[r]
    out_file = in_files$out_file[r]
    os_id = in_files$other_study_identifier[r]
    # Skip case for now
    if(os_id %in% skip_list){
      file.copy(f, out_file)
    }
    # Skip already processed
    if(file.exists(out_file)) next
    
    message("Working on file: ", f)
    tmp = extract_ntp_data_file(filepath = f)
    # Export extracted data using out file name
    writexl::write_xlsx(tmp, out_file)
  }
}
