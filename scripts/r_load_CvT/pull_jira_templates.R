# pull_jira_templates
# Jonathan Taylor Wall
# Created 2023-10-19
# Script to pull CvT templates directly from Jira for processing

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
        return()
    } else if(file_type == "xlsx"){
      sheet_ls = readxl::excel_sheets(temp_in)
      lapply(sheet_ls, function(s_name){
        readxl::read_xlsx(temp_in,
                          sheet=s_name)  
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
pull_jira_info <- function(jira_project, in_file = NULL, auth_token = NULL){
  
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
                  Status =="Done"
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

jira_download_templates <- function(in_data){
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

clowder_get_dataset_files <- function(dsID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)
  # Pull all Clowder Files from input dataset
  c_files_list = httr::GET(
    paste0(baseurl, "/api/datasets/", dsID,"/listAllFiles?limit=0"),
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()
  # Format data
  c_files_list = lapply(c_files_list, function(f){
    f %>%
      data.frame() %>%
      tidyr::unnest(cols=c())
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(clowder_id = id, `folders.name`, filename) %>%
    return()
}

upload_file_metadata <- function(metadata, dsID, userID, baseurl, apiKey){
  ################################################################################
  ### Push metadata
  ################################################################################
  
  c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey) %>%
    tidyr::unite(join_key, `folders.name`, filename, sep="_cvtjoin_")
  
  # Map metadata to Clowder Doc
  metadata = metadata %>%
    tidyr::unite(join_key, `Issue key`, filename, sep="_cvtjoin_") %>%
    dplyr::left_join(c_files_list,
                     by="join_key") %>%
    tidyr::separate(join_key, into = c("jira_ticket_id", "filename"), sep="_cvtjoin_") %>%
    dplyr::rename(jira_upload_date=date) %>%
    dplyr::select(-attachment_name, -filename) %>%
    dplyr::filter(!is.na(clowder_id))
    
  # Prep metadata JSON with userID
  md = list(
    "@context"= c(
      "https://clowder.ncsa.illinois.edu/contexts/metadata.jsonld",
      list("@vocab"= "https://clowder.ncsa.illinois.edu/contexts/metadata.jsonld")
    ),
    "agent"= list(
      "@type"= "cat:user",
      "user_id"= paste0("http://clowder-test.edap-cluster.com/api/users/", userID)
    ),
    "content"= c()
  )
  
  # add all metadata
  cat("...pushing metadata to files...\n")
  
  for(u in unique(metadata$clowder_id)){
    cat("...pushing Clowder File ", u, " (", which(metadata$clowder_id == u), " of ", length(unique(metadata$clowder_id)),")\n")
    content = metadata[metadata$clowder_id == u,]
    
    # Compare metadata
    old_metadata = clowder_get_file_metadata(fileID = u, baseurl, apiKey)
    
    compare_metadata = old_metadata %>%
      dplyr::select(any_of(names(content)))
    
    # Skip if old metadata matches new metadata
    if(identical(content, compare_metadata)) next
    
    # Create dictionary of metadata name value pairs
    md$content = metadata %>%
      dplyr::filter(clowder_id == u) %>%
      dplyr::select(-clowder_id) %>%
      purrr::flatten()
    # POST metadata for 'u' file
    # Rest between requests
    Sys.sleep(0.25)
    httr::POST(
      paste0(baseurl, "/api/files/",u ,"/metadata.jsonld"),
      httr::accept_json(),
      httr::content_type_json(),
      # Use API Key for authorization
      httr::add_headers(`X-API-Key` = apiKey),
      encode = "json",
      body=md
    )
  }
  cat("Done...\n")
}

clowder_get_file_metadata <- function(fileID, baseurl, apiKey){
  # Rest between requests
  Sys.sleep(0.25)
  
  # Format URL for request
  url = paste0(baseurl, "/api/files/metadata.jsonld?id=", 
               # Combine multiple file ID values if provided
               paste0(fileID, collapse="&id="), 
               "&?limit=0")
  # Pull metadata for input files
  metadata = httr::GET(
    url=url,
    httr::accept_json(),
    httr::content_type_json(),
    # Use API Key for authorization
    httr::add_headers(`X-API-Key` = apiKey),
    encode = "json"
  ) %>%
    httr::content()
  
  # Format data to return (combine across multiple metadata submissions)
  lapply(metadata, function(f){
    lapply(f, function(ff){
      tmp = ff %>%
        purrr::pluck("content") %>%
        purrr::compact() %>%
        data.frame() %>%
        # TODO Suppress new name print output
        tidyr::unnest(cols=c()) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.)))
    }) %>%
      dplyr::bind_cols()
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(clowder_id = names(metadata)) %>%
    return()
}

################################################################################
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

################################################################################
#' pull_clowder_files_to_load
#' Function to pull Clowder templates to process based on metadata "cvt_to_load"
pull_clowder_files_to_load <- function(dsID, baseurl, apiKey){
  # Pull full list of Clowder files in dataset
  c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey)
  
  # Split into chunks to pull metadata
  # Have to limit due to URL length constraints (see clowder_get_file_metadata)
  limit = 100
  nr <- nrow(c_files_list)
  to_load_files = split(c_files_list, rep(1:ceiling(nr/limit), each=limit, length.out=nr))
  # Loop through each chunk and filter to those marked as "cvt_to_load"
  to_load_files = lapply(seq_len(length(to_load_files)), function(i){
    # Add logic to chunk
    to_load_files[[i]] = clowder_get_file_metadata(fileID=c_files_list[[i]]$clowder_id, baseurl, apiKey)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(cvt_to_load == 1) %>%
    dplyr::select(clowder_id)
  
  # Filter original list to get ticket number and filename
  to_load_files = c_files_list %>%
    dplyr::filter(clowder_id %in% to_load_files$clowder_id) %>%
    return()
}
