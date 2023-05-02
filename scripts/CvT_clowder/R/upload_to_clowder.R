upload_to_clowder <- function(metadata=NULL, apiKey=NULL, datasetName=NULL, 
                              fileDir=NULL, type=NULL){
  baseurl = "https://clowder.edap-cluster.com/api"
  #Get dataset IDs of interest
  dataset_info <- httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets",
                                   apiKey)) %>% httr::content() %>%
    tibble(id = map_chr(.,"id"), name = map_chr(.,"name")) %>%
    select(-1) %>%
    filter(name %in% datasetName)#c("CvT Data Documents", "CvT Raw Extracted"))
  
  # file location to upload file from
  if(type=="datadocument"){
    fileLocs <- metadata %>% 
      select(filename) %>% 
      mutate(filename=paste0(fileDir, "/", filename))
  } else {
   fileLocs <- metadata %>%
     select(filename, `QA Category`, `QA Subcategory`) %>%
     unite(filename, `QA Category`, `QA Subcategory`, filename, sep="/", 
           na.rm=TRUE) %>% 
     mutate(filename=paste0(fileDir, "/", filename))
  }
  
  #get list of all files already uploaded.
  oldClowderFiles <- lapply(dataset_info$id, function(x){
    Sys.sleep(0.25)
    tmp = paste0(baseurl,"/datasets/",x,"/listAllFiles",apiKey)
    return(res <- GET(tmp) %>% 
             content() %>%
             tibble(filename = map_chr(.,"filename"), 
                    id = map_chr(.,"id")) %>% 
             select(-1)
    )
  }) %>% jsonlite::rbind_pages()
  
  # create url to upload with
  baseupload = paste0(baseurl, "/uploadToDataset/", 
                      dataset_info$id[grepl(datasetName,dataset_info$name)], 
                      apiKey)
  # upload file using POST command into correct dataset
  invisible(lapply(fileLocs$filename, function(y) {
    #Don't upload if file name already exists on Clowder
    if(!basename(y) %in% oldClowderFiles$filename){
      message("Trying to Upload: ", y)
      try(POST(baseupload, body = list(File = upload_file(y))))
      Sys.sleep(0.25)  
    }
  }))
  
  #get list of all files already uploaded.
  Sys.sleep(0.25)
  tmp = paste0(baseurl,"/datasets/",dataset_info$id,"/listAllFiles",apiKey)
  allfiles <- GET(tmp) %>% 
    content() %>%
    tibble(filename = map_chr(.,"filename"),
           id = map_chr(.,"id")) %>% 
    select(-1)# %>%
    #filter(!filename %in% oldClowderFiles$filename) #Filter to only new docs to add meta data to
  
  # add metadata ------------------------------------------------------------
  
  # get url for adding metadata to individual files
  filesurl <- paste0(baseurl,"/files")
  
  # below is an example of how to post with json-ld
  md = jsonlite::fromJSON('{
  "@context": [
    "https://clowder.ncsa.illinois.edu/contexts/metadata.jsonld",
    {"@vocab":"https://clowder.ncsa.illinois.edu/contexts/metadata.jsonld"}],
  "agent": {
    "@type": "cat:user",
    "user_id": ""
  },
  "content": {}
  }')
  #Add userID
  md$agent$user_id = userID
  
  # get metadta
  dat = metadata
  
  # join db table with clowder table
  res_uid = dat %>%
    #filter(uploadReady==1) %>%
    left_join(allfiles,
              by = "filename") %>%
    rename(clowder_uid = id) %>% 
    select(clowder_uid, everything()) %>% #Reorder to uid first
    filter(!is.na(clowder_uid))
  
  #pivot long
  res_long = res_uid %>%
    mutate_all(as.character) %>% pivot_longer(2:last_col())
  
  # add all metadata
  message("...pushing metadata to files...")
  invisible(lapply(unique(res_uid$clowder_uid), function(u){
    content <- res_long %>% filter(clowder_uid == u)
    md[["content"]] <- split(content$value,content$name)
    url <- paste0(filesurl,"/",u,"/metadata.jsonld",apiKey)
    if(compare_metadata(fileID=u, new_metadata=md$content, apiKey=apiKey)){#If the metadata is new, update it
      message("......Updating metadata for file: ", u)
      DELETE(url) #Delete old metadata if present
      Sys.sleep(0.25)
      POST(url,body = md, encode = 'json')
      Sys.sleep(0.25)  
    }
  }))
  #Attempt to move files into folders...
  # if(type == "extracted"){
  #  message("...Moving extracted templates to corresponding subfolders")
  #   #/datasets/{ds_id}/folders
  #   #Get list of folders in dataset
  #   folder_ids = httr::GET(paste0(baseurl,"/datasets/",dataset_info$id,"/folders",apiKey)) %>%
  #     content() %>%
  #     tibble(foldername = map_chr(.,"name"),
  #            id = map_chr(.,"id")) %>%
  #     select(-1)
  # 
  #   # upload file using POST command into correct dataset
  #   invisible(lapply(fileLocs$filename, function(y) {
  #     #Don't upload if file name already exists on Clowder
  #     #/datasets/{ds_id}/moveFile/{folder_id}/{file_id}:
  #     # create url to upload with
  #     #/datasets/ds_id/moveToDataset/folder_id/file_id
  #     newFolderID = ifelse(grepl("0_to_qa_format", y),
  #            folder_ids$id[grepl("0_to_qa_format",folder_ids$foldername)],
  #            "TBD")
  #     baseupload = paste0(baseurl, "/datasets/",
  #                         dataset_info$id[grepl(datasetName,dataset_info$name)],
  #                         "/moveFile/",newFolderID,"/",
  #                         allfiles$id[allfiles$filename == basename(y)],
  #                         apiKey)
  #       message("Trying to move file: ", y)
  #       try(POST(baseupload, body=list(folderId = newFolderID)))#paste0('{"folderId": "',newFolderID,'"}')))
  #       Sys.sleep(0.25)
  #   }))
  # }
  message("Done...")
}
