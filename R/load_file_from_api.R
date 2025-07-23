#' @title load_file_from_api
#' @description Function to load a CSV or XLSX file as a dataframe or list of dataframes using an API call. Typically, this is to load a CvTdb template file from Clowder.
#' @param url URL where the file will be downloaded.
#' @param headers Optional headers to complete the download call.
#' @param file_type String of the file extension. Currently only supports "csv" and "xlsx".
#' @param mode Download file mode, Default: 'w' for "write".
#' @return Dataframe or list of dataframes from a file downloaded from a URL.
#' @seealso 
#'  \code{\link[utils]{download.file}}
#'  \code{\link[readr]{read_delim}}, \code{\link[readr]{cols}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[readxl]{excel_sheets}}, \code{\link[readxl]{read_excel}}
#' @rdname load_file_from_api
#' @export 
#' @importFrom utils download.file
#' @importFrom readr read_csv cols
#' @importFrom dplyr mutate across
#' @importFrom stringr str_squish
#' @importFrom readxl excel_sheets read_xlsx
load_file_from_api <- function(url, headers, file_type, mode = "w"){
  temp_in <- tempfile(fileext = paste0(".", file_type))
  out <- tryCatch({ 
    utils::download.file(url = url, 
                         destfile = temp_in,
                         headers = headers,
                         mode = mode)
    if(file_type == "csv"){
      readr::read_csv(temp_in, 
                      col_types = readr::cols()) %>%
        dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish)) %>%
        return()
    } else if(file_type == "xlsx"){
      sheet_ls = readxl::excel_sheets(temp_in)
      lapply(sheet_ls, function(s_name){
        readxl::read_xlsx(temp_in,
                          sheet=s_name) %>%
          dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish))
      }) %T>% {
        names(.) <- sheet_ls
      }
    } else {
      stop("'load_file_from_api()' unsupported file_type '", file_type,"'")
    }
  }, error=function(e) {
    message(e)
    return(NULL)
  }, 
  finally = { unlink(temp_in) }
  )  
  return(out)
}
