#' @description A helper function to check if a file has logged issues (changed to 
#' 1 for select columns) and move it to appropriate subfolder.
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx]
#' @rdname reorganize_file_flags
#' @export 
#' @importFrom readxl read_xlsx
reorganize_file_flags <- function(){
  log = readxl::read_xlsx("output\\template_normalization_log.xlsx")
  flag_map = readxl::read_xlsx("input\\dictionaries\\flag_map.xlsx")
  for(i in seq_len(nrow(log))){
    # if(i <= 7){#Quick skip/restart logic
    #   next
    # }
    f = log$filename[i]
    if(file.exists(paste0("output/normalized_templates/",gsub(".xlsx", "_normalized.xlsx", basename(f))))){
      for(flag in unique(flag_map$`Flag Type`)){
        # Get flag subfolder path
        f_path = switch(flag,
                      "Warning" = "output/normalized_templates/flagged/warning/",
                      "Hard Stop (Missing Required)" = "output/normalized_templates/flagged/hard_stop/missing_required/",
                      "Hard Stop (Need Split)" = "output/normalized_templates/flagged/hard_stop/need_split/",
                      "Hard Stop (Impossible Value)" = "output/normalized_templates/flagged/hard_stop/impossible_value/",
                      "Hard Stop (Conversion Failed)" = "output/normalized_templates/flagged/hard_stop/conversion_failed/",
                      "Hard Stop (Empty Sheet)" = "output/normalized_templates/flagged/hard_stop/empty_sheet/",
                      "Soft Stop (Conversion Needed)" = "output/normalized_templates/flagged/soft_stop/conversion_needed/",
                      "Soft Stop (Dictionary Update)" = "output/normalized_templates/flagged/soft_stop/dictionary_update/",
                      "Soft Stop (Clowder Doc Missing)" = "output/normalized_templates/flagged/soft_stop/clowder_missing/"
        )
        if(!is.null(f_path)){
          # Check if file was flagged with any corresponding flag for a category
          if(any(
            log[log$filename == f, 
                    names(log)[names(log) %in% flag_map$`Field Name`[flag_map$`Flag Type` == flag]]  
                     ]
            )
            ){
            message("Moving file to: ", f_path)
            file.rename(from=paste0("output/normalized_templates/",gsub(".xlsx", "_normalized.xlsx", basename(f))),
                        to=paste0(f_path, gsub(".xlsx", "_normalized.xlsx", basename(f)))
            )
            break #If moved, skip any further checks
          }
        }
      }
    }
  }
}
