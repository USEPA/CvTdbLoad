#' pull_clowder_files_to_load
#' Function to pull Clowder templates to process based on metadata "cvt_to_load"
pull_clowder_files_to_load <- function(dsID, baseurl, apiKey, curation_set_tag, metadata_filter_tag=NULL){
  # Pull full list of Clowder folders to help with data provenance labeling
  c_folders_list <- clowder_get_dataset_folders(dsID, baseurl, apiKey) %>%
    dplyr::mutate(foldername = foldername %>%
                    gsub("^\\/", "", .)) %>%
    tidyr::separate(foldername, into=c("curation_set_tag", "jira_ticket"), sep="/", 
                    extra="merge", fill="right")
  # Pull full list of Clowder files in dataset with "normalized"  stem
  c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey) %>%
    dplyr::rename(jira_ticket=folders.name) %>%
    dplyr::left_join(c_folders_list,
                     by="jira_ticket")
  
  # Apply dataset filter if provided
  if(!is.null(curation_set_tag) && !is.na(curation_set_tag) && curation_set_tag != ''){
    c_files_list = c_files_list %>%
      dplyr::filter(curation_set_tag == !!curation_set_tag, !is.na(jira_ticket))
  }
  
  if(nrow(c_files_list)){
    # Filter to a specific file metadata filter tag if provided
    if(length(metadata_filter_tag)){
      # Split into chunks to pull metadata
      # Have to limit due to URL length constraints (see clowder_get_file_metadata)
      limit = 100
      nr <- nrow(c_files_list)
      to_load_files = split(c_files_list, rep(1:ceiling(nr/limit), each=limit, length.out=nr))
      # Loop through each chunk and filter to those marked as "cvt_to_load"
      to_load_files = lapply(seq_len(length(to_load_files)), function(i){
        # Add logic to chunk
        to_load_files[[i]] = clowder_get_file_metadata(fileID=to_load_files[[i]]$clowder_id, baseurl, apiKey) %>%
          dplyr::select(clowder_id, any_of(contains(metadata_filter_tag))) %>%
          tidyr::pivot_longer(-clowder_id) %>%
          dplyr::filter(!is.na(value))
      }) %>%
        dplyr::bind_rows() %>%
        # dplyr::select(clowder_id) %>%
        dplyr::distinct()
      
      # Filter original list to get ticket number and filename
      c_files_list %>%
        dplyr::filter(clowder_id %in% to_load_files$clowder_id) %>%
        return() 
    } else {
      c_files_list %>%
        # dplyr::select(clowder_id) %>%
        dplyr::distinct() %>%
        return()
    }
  } else {
    message("No Clowder file records to return...")
    return(data.frame())
  }
}