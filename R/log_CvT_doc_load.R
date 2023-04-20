#' @title log_CvT_doc_load
#' @description FUNCTION_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param m PARAM_DESCRIPTION, Default: NULL
#' @param reset PARAM_DESCRIPTION, Default: FALSE
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
log_CvT_doc_load <- function(f, m=NULL, reset=FALSE){
  if(file.exists("output\\template_normalization_log.xlsx")){
    log = readxl::read_xlsx("output\\template_normalization_log.xlsx")
    log$timestamp = as.character(log$timestamp)  
  } else {
    log = data.frame(filename=f, timestamp=as.character(Sys.time()))
  }
  
  #Add a new flag column if it doesn't exist
  if(!is.null(m)){
    if(!m %in% names(log)){
      log[[m]] <- 0
    }  
  }
  if(f %in% log$filename){
    if(reset){#Reset to 0 for entry
      log[log$filename == f, names(log)[!names(log) %in% c("filename", "timestamp")]] <- 0
    }
    if(!is.null(m)){
      #Set new flag
      log[log$filename == f, m] <- 1
      log[log$filename == f, "timestamp"] <- as.character(Sys.time())  
    }
  } else {
    tmp = stats::setNames(data.frame(matrix(ncol = length(log), nrow = 1)), names(log))
    tmp[, names(tmp)[!names(tmp) %in% c("filename", "timestamp")]] <- 0
    tmp$filename = f
    if(!is.null(m)){
      tmp[m] = 1  
    }
    tmp$timestamp <- as.character(Sys.time())
    log = rbind(log, tmp)
  }
  writexl::write_xlsx(log, "output\\template_normalization_log.xlsx")
}
