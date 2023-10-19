# pull_jira_templates
# Jonathan Taylor Wall
# Created 2023-10-19
# Script to pull CvT templates directly from Jira for processing

load_file_from_api <- function(url, headers, file_type){
  temp_in <- tempfile()
  out <- tryCatch({ 
    utils::download.file(url = url, 
                         destfile = paste0(temp_in, ".", file_type),
                         headers = headers)
    if(file_type == "csv"){
      readr::read_csv(temp_in, 
                      col_types = readr::cols()) %>%
        return()
    } else if(file_type == "xlsx"){
      readxl::read_xlsx(temp_in)
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

#' @title pull_jira_templates
#' @description Script to process CSV export of Jira into a status log
#' @param jira_project Jira project code (e.g. CVTDB)
#' @param download_bulk Boolean whether to bulk download ticket attachments, Default: FALSE.
#' @param reset_download Boolean whether to re-download ticket attachments, Default: FALSE.
#' @param auth_token Authorization token for Jira
#' @return Summary DataFrame of Jira tickets by Epic, Label, and Status
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  out = jira_summary_bulk_download(jira_project="project_name", download_bulk = FALSE, reset_download = FALSE)
#'  }
#' }
#' @seealso 
#'  [download.file][utils::download.file], [unzip][utils::unzip]
#'  [read_csv][readr::read_csv], [cols][readr::cols]
#'  [select][dplyr::select], [contains][dplyr::contains], [mutate][dplyr::mutate], [everything][dplyr::everything], [filter][dplyr::filter], [distinct][dplyr::distinct], [left_join][dplyr::left_join], [group_by][dplyr::group_by], [summarise][dplyr::summarise], [n][dplyr::n]
#'  [unite][tidyr::unite]
#'  [str_squish][stringr::str_squish]
#' @rdname jira_summary_bulk_download
#' @export 
#' @importFrom utils download.file unzip
#' @importFrom readr read_csv cols
#' @importFrom dplyr select contains mutate everything filter distinct left_join group_by summarise n
#' @importFrom tidyr unite
#' @importFrom stringr str_squish
pull_jira_templates <- function(jira_project, in_file = NULL, download_bulk = FALSE, reset_download = FALSE, auth_token = NULL){
  
  # Format headers
  if(!is.null(auth_token)){
    headers <- c(Authorization = paste0("Bearer ", auth_token))
  } else {
    headers <- NULL
  }
  
  if(is.null(in_file)){
    # Pull CSV export from Jira
    url = paste0("https://jira.epa.gov/sr/jira.issueviews:searchrequest-csv-all-fields/temp/SearchRequest.csv?jqlQuery=project+%3D+", jira_project)
    in_data_url <- load_file_from_api(url=url, headers=headers, file_type="csv")
  } else {
    # Load input file
    in_data_url <- readr::read_csv(in_file, 
                                   col_types = readr::cols())
  }
  
  # Check if input Jira file loaded
  if(is.null(in_data_url)){
    stop("Either could not pull directly from Jira or 'in_file' loading error...")
  }
  
  # Process loaded data
  in_data <- in_data_url %>%
    dplyr::select(Summary, 
                  `Issue key`,	
                  `Issue id`,
                  `Issue Type`,
                  Status,
                  Created,
                  dplyr::contains("Labels"),
                  Description,
                  `Epic Link`=`Custom field (Epic Link)`) %>%
    # Join labels
    tidyr::unite(dplyr::contains("Labels"), col="Labels", sep=", ", na.rm = TRUE) %>%
    # Remove extraneous label
    dplyr::mutate(Labels = gsub(", PKWG", "", Labels)) %>%
    dplyr::select(`Issue key`, dplyr::everything())
  
  # Replace empty strings with NA
  in_data$Labels[in_data$Labels == ""] <- NA
  # Rename Epic Link with Epic Names
  epics <- in_data %>%
    dplyr::filter(`Issue Type` == "Epic") %>%
    dplyr::select(`Issue key`, `Epic Name` = Summary) %>%
    dplyr::distinct()
  
  in_data <- in_data %>%
    dplyr::left_join(epics,
                     by=c("Epic Link"="Issue key")) %>%
    dplyr::filter(`Epic Name` == "Document Curation",
                  `Issue Type` != "Epic",
                  Status =="Done") %>%
    dplyr::select(-`Issue Type`) %>%
    # Clean labels for better subfolder grouping
    dplyr::mutate(Labels = Labels %>%
                    gsub(", re-extraction", "", .) %>% 
                    gsub("Extraction-Corrected,", "", .) %>%
                    stringr::str_squish())
  
  # Fill in Labels
  in_data$Labels[is.na(in_data$Labels)] <- "Misc"
  
  out_summary <- in_data %>%
    dplyr::group_by(`Epic Name`, Labels, Status) %>%
    dplyr::summarise(n = dplyr::n(),
                     .groups="keep")
  
  # Filter to templates to load
  ticket_attachment_metadata <- in_data_url %>%
    dplyr::filter(`Issue key` %in% in_data$`Issue key`) %>%
    dplyr::select(`Issue key`, dplyr::starts_with("Attachment")) %>%
    tidyr::pivot_longer(cols=dplyr::starts_with("Attachment"),
                        names_to="attachment_name") %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::separate(value, sep=";",
                    into = c("date", "uploaded_by", "filename", "jira_link")) %>%
    dplyr::mutate(file_ext = tools::file_ext(filename),
                  attachment_name = attachment_name %>%
                    gsub("Attachment...", "", .) %>%
                    as.numeric())
  
  # Test loading Clowder file directly
  # 65318469e4b045b9ff7b00d8
  tmp = load_file_from_api(url = paste0("https://clowder.edap-cluster.com/api/files/65318469e4b045b9ff7b00d8"),
                     headers <- c(`X-API-Key` = apiKey),
                     file_type = "xlsx")
  
  downloader::download(paste0("https://clowder.edap-cluster.com/api/files/", 
                              "65318469e4b045b9ff7b00d8", "?apiKey=", apiKey), 
                       paste0("output/", 
                              #sub("^.*?([A-Z])", "\\1", 
                              "test_clowder_file.xlsx"
                              #   )
                       ), #Remove starting hashkey string before first capitalization
                       mode = "wb", 
                       quiet=TRUE)
  
  
  ################################################################################
  ### Bulk Download
  ################################################################################
  if(download_bulk){
    bulk_download <- in_data # %>%
    # filter(`Epic Name` %in% epics_to_download,
    #        !Status %in% c("Ice Box", "Rejected"))
    
    # epics_to_download <- c("Document Curation", "CVTDB QC")
    epics_to_download <- paste0("output/", bulk_download$`Epic Name`) %>% unique()
    
    bulk_download <- bulk_download %>%
      tidyr::unite(col="destdir", `Epic Name`, Labels, sep="/", remove=FALSE) %>%
      dplyr::mutate(destdir = paste0("output/", destdir))
    
    # Make directories by Epic
    for(epic in epics_to_download){
      # Make if not exist
      if(!dir.exists(epic)) dir.create(epic)
    }
    
    # Make subdirectories by Label
    for(d_dir in unique(bulk_download$destdir)){
      if(!dir.exists(d_dir)) dir.create(d_dir, recursive = TRUE)
    }
    
    # Bulk download and unzip files
    for(i in seq_len(nrow(bulk_download))){
      message("Working on ticket ", i, " of ", nrow(bulk_download))
      destfile <- paste0(bulk_download$destdir[i], "/", bulk_download$`Issue key`[i],".zip")
      zipDir <- destfile %>% gsub(".zip", "", .)
      
      # Check if already downloaded/unzipped
      if(dir.exists(zipDir)) next
      
      # Unzip already downloaded but not unzipped .zip
      if(!file.exists(destfile)){
        url <- paste0("https://jira.epa.gov/secure/attachmentzip/",bulk_download$`Issue id`[i],".zip")
        
        # Download files as subdirectories
        # Bulk download of ticket zip files
        # jira.epa.gov/secure/attachmentzip/*key*.zip
        tryCatch(
          {
            utils::download.file(url = url, 
                                 destfile = destfile,
                                 method = "libcurl",
                                 headers = headers)
          },
          error=function(cond) {
            message("Problem with ticket: ", bulk_download$`Issue key`[i])
            message(cond)
          },
          warning=function(cond) {
            message("Warning with ticket: ", bulk_download$`Issue key`[i])
            message(cond)
          }
        ) 
      }
      
      # Unzip files
      tryCatch(
        {
          # Only unzip if exists
          if(file.exists(destfile)){
            utils::unzip(destfile, exdir=zipDir)
          }
        },
        error=function(cond) {
          message("Problem with ticket: ", bulk_download$`Issue key`[i])
          message(cond)
        },
        warning=function(cond) {
          message("Warning with ticket: ", bulk_download$`Issue key`[i])
          message(cond)
        }
      ) 
      
      # Only unzip if exists
      if(file.exists(destfile)){
        file.remove(destfile) 
      }
    }
  }
  
  return(out_summary)
}
