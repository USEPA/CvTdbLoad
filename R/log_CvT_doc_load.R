#' @title log_CvT_doc_load
#' @description FUNCTION_DESCRIPTION
#' @param f Filename to flag.
#' @param m Log field name, Default: NULL
#' @param reset Boolean to reset a row's flags, Default: FALSE
#' @param val Custom field value. Default: NULL
#' @param log_path File path where to save the log file. Default "output/template_normalization_log.xlsx"
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
#'  [setNames][stats::setNames]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname log_CvT_doc_load
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames
#' @importFrom writexl write_xlsx
log_CvT_doc_load <- function(f, 
                             m=NULL, 
                             reset=FALSE, 
                             val=NULL,
                             log_path = "output/template_normalization_log.xlsx"){
  if(file.exists(log_path)){
    log = readxl::read_xlsx(log_path)
    log$timestamp = as.character(log$timestamp)  
  } else {
    log = data.frame(filename=f, timestamp=as.character(Sys.time()))
  }
  
  #Add a new flag column if it doesn't exist
  if(!is.null(m)){
    if(!m %in% names(log)){
      log[[m]] <- "0"
    }  
  }
  if(f %in% log$filename){
    if(reset){#Reset to 0 for entry
      log[log$filename %in% f, names(log)[!names(log) %in% c("filename", "timestamp")]] <- "0"
    }
    if(!is.null(m)){
      #Set new flag
      if(!is.null(val)){
        log[log$filename %in% f, m] <- toString(val)
      } else {
        log[log$filename %in% f, m] <- "-1"
      }
      log[log$filename %in% f, "timestamp"] <- as.character(Sys.time())  
    }
  } else {
    tmp = stats::setNames(data.frame(matrix(ncol = length(log), nrow = 1)), names(log))
    tmp[, names(tmp)[!names(tmp) %in% c("filename", "timestamp")]] <- "0"
    tmp$filename = f
    if(!is.null(m)){
      if(!is.null(val)){
        tmp[m] = toString(val)
      } else {
        tmp[m] = "-1"
      }
    }
    tmp$timestamp <- as.character(Sys.time())
    log = rbind(log, tmp)
  }
  writexl::write_xlsx(log, log_path)
}
