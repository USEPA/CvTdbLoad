jira_download_templates <- function(in_data, auth_token){
  # Format headers
  if(!is.null(auth_token)){
    headers <- c(Authorization = paste0("Bearer ", auth_token))
  } else {
    headers <- NULL
  }
  ################################################################################
  ### Bulk Download
  ################################################################################
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