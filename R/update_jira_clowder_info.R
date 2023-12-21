update_jira_clowder_info <- function(jira_project, in_file, auth_token, reset_attachments=FALSE, dsID, baseurl, userID, apiKey){
  # Pull initial Jira information
  jira_info = pull_jira_info(jira_project=jira_project,
                             in_file=in_file,
                             auth_token=auth_token)
  # Download attachments as needed
  # Pull all
  if(reset_attachments){
    jira_download_templates(in_data = jira_info$in_data)
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
      message("Working on ticket ", which(bulk_download$filename == filename), " of ", nrow(bulk_download))
      # Set destination file name
      destfile <- paste0(bulk_download$destdir[bulk_download$filename == filename], "/", 
                         filename)
      # Download if does not exist
      if(!file.exists(destfile)){
        utils::download.file(url = bulk_download$jira_link[bulk_download$filename == filename],
                             destfile = destfile,
                             headers = c(`X-API-Key` = apiKey),
                             mode = "wb")  
      }
    }
  }
  
  upload_file_metadata(metadata=metadata, 
                       dsID=dsID, 
                       userID=userID, 
                       baseurl=baseurl, 
                       apiKey=apiKey)
}