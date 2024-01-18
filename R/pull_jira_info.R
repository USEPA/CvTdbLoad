#' @title pull_jira_info
#' @description Script to process CSV export of Jira into a status log
#' @param jira_project Jira project code (e.g. CVTDB)
#' @param download_bulk Boolean whether to bulk download ticket attachments, Default: FALSE.
#' @param auth_token Authorization token for Jira
#' @return Summary DataFrame of Jira tickets by Epic, Label, and Status
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  out = pull_jira_info(jira_project="project_name")
#'  }
#' }
#' @seealso 
#'  [download.file][utils::download.file], [unzip][utils::unzip]
#'  [read_csv][readr::read_csv], [cols][readr::cols]
#'  [select][dplyr::select], [contains][dplyr::contains], [mutate][dplyr::mutate], [everything][dplyr::everything], [filter][dplyr::filter], [distinct][dplyr::distinct], [left_join][dplyr::left_join], [group_by][dplyr::group_by], [summarise][dplyr::summarise], [n][dplyr::n]
#'  [unite][tidyr::unite]
#'  [str_squish][stringr::str_squish]
#' @rdname pull_jira_info
#' @export 
#' @importFrom utils download.file unzip
#' @importFrom readr read_csv cols
#' @importFrom dplyr select contains mutate everything filter distinct left_join group_by summarise n
#' @importFrom tidyr unite
#' @importFrom stringr str_squish
pull_jira_info <- function(jira_project, in_file = NULL, auth_token = NULL, status_filter = "Done"){
  
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
                  Status %in% c(status_filter)
    ) %>%
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
                    as.numeric()) %>%
    dplyr::left_join(in_data %>% 
                       select(`Issue key`, `Epic Name`, Labels),
                     by = "Issue key")
  
  return(list(in_data=in_data,
              out_summary=out_summary,
              ticket_attachment_metadata=ticket_attachment_metadata))
}