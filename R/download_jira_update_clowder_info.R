#' @title download_jira_update_clowder_info
#' @description Function to generate a dataframe of Jira ticket metadata with attachment information. Can be used to download the attachments locally and upload metadata to Clowder files once they have been uploaded to Clowder separately.
#' @param jira_project Jira project identifier (e.g., CVTDB).
#' @param in_file Optional param for the file path to previously pulled Jira information CSV file.
#' @param auth_token Jira API token.
#' @param update_clowder_metadata Boolean whether to update Clowder file metadata. Only set to TRUE once Jira ticket attachments have been uploaded to Clowder. Default: FALSE.
#' @param reset_attachments Boolean whether to re-download Jira ticket attachments. Default: FALSE.
#' @param dsID Clowder dataset identifier.
#' @param baseurl Clowder base URL.
#' @param userID Clowder user identifier.
#' @param apiKey Clowder API key.
#' @param labels_filter Vector list of Jira ticket labels to filter to.
#' @param epic_filter Custom filtering to a specific ticket Epic link by name (single or vector). Default: empty vector.
#' @param attachment_filter Filename regex string vector to filter to select Jira ticket attachments, Default: empty vector.
#' @return Dataframe of metadata from Jira tickets to associate to Clowder files.
#' @seealso 
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{unite}}
#'  \code{\link[utils]{View}}, \code{\link[utils]{download.file}}
#' @rdname download_jira_update_clowder_info
#' @export 
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr unite
#' @importFrom utils download.file
download_jira_update_clowder_info <- function(jira_project, 
                                              in_file = NULL, 
                                              auth_token, 
                                              reset_attachments=FALSE, 
                                              update_clowder_metadata=FALSE,
                                              dsID, 
                                              baseurl, 
                                              userID, 
                                              apiKey,
                                              labels_filter = NULL,
                                              epic_filter = c(),
                                              attachment_filter = c()){
  # Pull initial Jira information
  jira_info = pull_jira_info(jira_project=jira_project,
                             in_file=in_file,
                             auth_token=auth_token,
                             epic_filter=epic_filter)
  # Filter labels if provided
  if(!is.null(labels_filter)){
    jira_info$in_data = jira_info$in_data %>%
      dplyr::filter(Labels %in% labels_filter)
    jira_info$ticket_attachment_metadata = jira_info$ticket_attachment_metadata %>%
      dplyr::filter(Labels %in% labels_filter)
    jira_info$out_summary = jira_info$out_summary %>%
      dplyr::filter(Labels %in% labels_filter)
  }
  
  # Filter to specified filename regex
  if(length(attachment_filter)){
    jira_info$ticket_attachment_metadata = jira_info$ticket_attachment_metadata %>%
      dplyr::filter(grepl(paste0(attachment_filter, collapse = "|"), filename))
    jira_info$in_data = jira_info$in_data %>%
      dplyr::filter(`Issue key` %in% unique(jira_info$ticket_attachment_metadata$`Issue key`))
  }
  
  # Download attachments as needed
  # Pull all
  if(!reset_attachments){
    jira_download_templates(in_data = jira_info$in_data, auth_token = auth_token)
    metadata = jira_info$ticket_attachment_metadata
  } else {
    # Only pull those not already on Clowder
    ds_file_list = clowder_get_dataset_files(dsID=dsID,
                                             baseurl=baseurl,
                                             apiKey=apiKey)
    bulk_download = jira_info$ticket_attachment_metadata %>%
      dplyr::filter(!filename %in% ds_file_list$filename)
    metadata = bulk_download %>% 
      dplyr::select(-any_of(c("destdir", "Epic Link", "Epic Name")))
    
    # Set up directory
    # epics_to_download <- c("Document Curation", "CVTDB QC")
    epics_to_download <- paste0("output/", bulk_download$`Epic Name`) %>% unique()
    
    bulk_download <- bulk_download %>%
      tidyr::unite(col="destdir", `Epic Name`, Labels, `Issue key`, sep="/", remove=FALSE) %>%
      dplyr::mutate(destdir = paste0("output/", destdir))
    
    # Make directories by Epic
    for(epic in epics_to_download){
      # Make if not exist
      if(!dir.exists(epic)) dir.create(epic)
    }
    
    # Make subdirectories by Label and Jira ticket
    for(d_dir in unique(bulk_download$destdir)){
      if(!dir.exists(d_dir)) dir.create(d_dir, recursive = TRUE)
    }
    
    # Iterate through files not on Clowder to download
    for(filename in bulk_download$filename){
      if(length(which(bulk_download$filename == filename)) > 1){
        message("...Duplicate filename found: ", filename)
        # bulk_download %>% dplyr::filter(filename == !!filename) %>% View()
        browser()
        next
      }
      message("Working on ticket ", which(bulk_download$filename == filename), " of ", nrow(bulk_download))
      # Set destination file name
      destfile <- paste0(bulk_download$destdir[bulk_download$filename == filename], "/", 
                         filename)
      # Download if does not exist
      if(!file.exists(destfile)){
        # Wait between requests
        Sys.sleep(0.25)
        utils::download.file(url = bulk_download$jira_link[bulk_download$filename == filename],
                             destfile = destfile,
                             headers = c(Authorization = paste0("Bearer ", auth_token)),
                             mode = "wb")  
      }
    }
  }

  if(update_clowder_metadata){
    # Update Clowder Jira file metadata
    upload_file_metadata(metadata=metadata,
                         dsID=dsID,
                         userID=userID,
                         baseurl=baseurl,
                         apiKey=apiKey)  
  }
  return(metadata)
}
