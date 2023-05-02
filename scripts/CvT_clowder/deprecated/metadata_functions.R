library(tidyr); library(dplyr); library(googlesheets4); library(easyPubMed); library(purrr) #Need version 2.13 for easyPubMed for improved/accurate output (dev version)

#'@title Create Clowder Metadata File
#'@description This is a function to compile metadata for CvT extracted templates and PDF files.
#'@param outputName The name of the metadata file to save (.csv).
#'@param type The type of metadata file to create ('datadocument' or 'extracted').
#'@param fielDir The file directory path to pull documents from to create metadata for.
#'@param gsURL The URL to a GoogleSheets to pull curation notes for extracted templates.
#'@param metaCurated A boolean of whether to return the metadata dataframe (if it's ready).
#'#'@param apiKey The API key required for a user to access the Clowder dataset.
#'@return A dataframe of the metadata if metaCurated is TRUE.
#'@import readr
create_meta_data_file <- function(outputName = "", type="", fileDir="", gsURL="", 
                                  metaCurated=FALSE, apiKey = NULL){
  if(file.exists(outputName)){
    message("\nLoading metadata file already created for type: ", type)
    origdf = readr::read_csv(outputName, col_types = readr::cols())
    message("...Appending any new files...")
  } else {
    origdf = NULL
    message("...Creating template metadata file to populate: ", outputName)
  }
  if(type == "datadocument"){
    tmp = data.frame(filename = list.files(fileDir, pattern=".pdf", recursive = TRUE, 
                                           include.dirs = TRUE) %>% 
                       unique())  
  } else {
    tmp = data.frame(filename = list.files(fileDir, pattern=".xlsx", recursive = TRUE, 
                                           include.dirs = TRUE) %>%
                       unique()) %>%
      filter(!filename %in% c("CvT QA Outline.docx", "qa_log.xlsx")) %>%
      organize_directory_groups() %>%
      check_pdf_loaded_clowder(apiKey=apiKey)
  }
  
  if(!is.null(origdf)){
    message("...Filtering to new files to append")
    tmp = tmp %>% filter(!filename %in% origdf$filename)
  }
  if(nrow(tmp)){
    if(type == "extracted"){
      #Parsing filename into PMID and Curator Initials (standardized filenames)
      tmp = tmp %>%
        mutate(PMID = as.numeric(PMID)) %>%
        select(PMID, filename, `QA Category`, `QA Subcategory`, `Curator Initials`, 
               `PDF in Clowder`, uploadReady) %>%
        filter(!is.na(PMID))
    } else if(type == "datadocument"){
      message("...Prepping new file data to append")
      #filename, Author, Publication Year, PMID, Article Title, Curator Notes, CvT Present, Figs_Tbls Extracted
      tmp$PMID = gsub("\\.pdf", "", tmp$filename) %>% 
        gsub("PMID", "", .) %>% 
        trimws()
      #https://cran.r-project.org/web/packages/easyPubMed/vignettes/getting_started_with_easyPubMed.html
      tmp2 = lapply(tmp$PMID, function(x){
        tmp3 = x %>%
          get_pubmed_ids()
        if(tmp3$Count != 1){
          message("......No PMID found for: ", x)
        } else {
          fetch_pubmed_data(tmp3, encoding = "ASCII") %>%
            table_articles_byAuth(pubmed_data = .,
                                  included_authors = "first",
                                  max_chars = 100,
                                  autofill = TRUE) %>%
            select(PMID=pmid, `Publication Year`=year, `Author Lastname`=lastname, 
                   `Author Firstname`=firstname, `Article Title`=title, journal)
        }
      }) %>% purrr::compact() %>% jsonlite::rbind_pages()
      if(nrow(tmp2)){
        tmp = left_join(tmp, tmp2, by="PMID")
      } else {
        message("...No PubMed Data found...creating blank columns")
        tmp[,c("Publication Year", "Author Lastname", "Author Firstname", 
               "Article Title", "journal")] <- ""  
      }
      rm(tmp2)
    }
  } else {
    message("...No new files to append...returning metadata")
    return(readr::read_csv(outputName, col_types = readr::cols()))
  }

  if(!is.null(origdf)){ tmp = rbind(origdf, tmp) }
  
  if(type == "extracted"){
    message("...Pulling Curation Notes")
    if(!is.numeric(tmp$PMID)){#PMID data type handling
      curationNotes = googlesheets4::read_sheet(gsURL, sheet="Coordination", 
                                                skip=10, col_types = "ccccccccc") %>%
        select(PMID=pmid, #`Curator Initials`=`Curator 1`, 
               `Curator Notes`=`Curator1 Notes`, 
               `Curation Finish Date`=`Curator 1 Finish Date`) %>%
        filter(PMID %in% tmp$PMID, !is.na(PMID))#, !is.na(`Curator Initials`))
    } else {
      curationNotes = googlesheets4::read_sheet(gsURL, sheet="Coordination", 
                                                skip=10, col_types = "ncccccccc") %>%
        select(PMID=pmid, #`Curator Initials`=`Curator 1`, 
               `Curator Notes`=`Curator1 Notes`, 
               `Curation Finish Date`=`Curator 1 Finish Date`) %>%
        filter(PMID %in% tmp$PMID, !is.na(PMID))#, !is.na(`Curator Initials`))  
    }
    #Recombine curationNotes, even if some were present previously. Assumes Google Sheets are the most updated version
    tmp = left_join(tmp[!names(tmp) %in% c("Curator Notes", "Curation Finish Date")],
                    curationNotes, by="PMID")# %>%
      #mutate(`Curator Initials`=gsub("[^A-Z]*([A-Z])[^A-Z]*", "\\1", `Curator Initials`)) #Convert full names to initials  
  } #else {
  #  #Recombine curator initials, even if some were present previously. Assumes Google Sheets are the most updated version
  #  tmp = left_join(tmp %>% select(-`Curator Initials`), 
  #                  curationNotes %>% select(PMID, `Curator Initials`), by="PMID") %>%
  #    mutate(`Curator Initials`=gsub("[^A-Z]*([A-Z])[^A-Z]*", "\\1", `Curator Initials`)) #Convert full names to initials
  #}
  
  #Check for duplicates
  dups = tmp %>% distinct() %>% 
    group_by(PMID, filename) %>% 
    summarise(n = n()) %>% filter(n > 1, !is.na(PMID), !PMID %in% c("")) %>%
    ungroup()
  if(type == "extracted"){#Handle duplicates for extracted templates by Finish Date
    tmp2 = anti_join(tmp, dups, by=c("PMID", "filename")) #Remove duplicates
    dups2 = left_join(dups, tmp, by=c("PMID", "filename")) %>% #Filter to ones with finish dates
      filter(!is.na(`Curation Finish Date`)) %>%
      select(-n)
    tmp = rbind(tmp2, dups2)  
  }
  
  if(nrow(dups)){
    warning("...Duplicate PMIDs introduced.\nDuplicates: ", 
            paste0(dups$PMID, sep=", "))
  }
  message("...Writing output")
  readr::write_csv(tmp %>% distinct(), outputName)
  ifelse(metaCurated, return(tmp %>% distinct()), return(NULL))
}


#'@title Download Clowder Documents by File ID and Filename
#'@description This is a helper function to download documents using the Clowder API. It also checks if a fileID/fileName has already been downloaded and skips if it exists
#'@param docData Dataframe containing fileID and fileName columns for the Clowder API download
#'@param fileName A character list of file names (with file extension) to name the downloaded files
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@param limit A numeric integer value to specify the number of documents to pull from the list of file IDs (Leave NULL if you want all to be downloaded)
#'@return None. Files are downloaded into specified output directory
#'@import downloader
#'@export
download_clowder_docs <- function(docData=NULL, outputDir, apiKey, limit=NULL){
  if(is.null(docData)) stop("Error: Must provide docData to download clowder docs")
  #Filter list by what is already downloaded in outputDir
  tmp <- list.files(outputDir)
  #Remove fileId and fileNames from already downloaded docs
  fileName <- docData$filename[!(docData$filename %in% tmp)] #Filter out fileNames where fileNames have already been downloaded
  fileID <- docData$id[which(docData$filename %in% fileName)] #Filter out fileIDs where fileNames have already been downloaded  
  
  if(is.null(limit)){ limit <- length(fileID) } #No limit input, pull all documents
  message("Downloading ", limit, " new docs from Clowder API...")
  if(limit){
    invisible(lapply(seq_len(limit), function(x){
      tryCatch({
        downloader::download(paste0("https://clowder.edap-cluster.com/api/files/", 
                                    fileID[x], apiKey), 
                             paste0(outputDir, 
                                    #sub("^.*?([A-Z])", "\\1", 
                                    fileName[x]
                                    #   )
                             ), #Remove starting hashkey string before first capitalization
                             mode = "wb", 
                             quiet=TRUE)
      },
      error=function(cond){ message("Error message: ", cond); return(NA) },
      warning=function(cond){ message("Warning message: ", cond); return(NULL) }
      )
    }))
  }
}

#'@title Organize Clowder Extracted Templates by Directory
#'@description This is a helper function to organize extracted templates by their subdirectory groups.
#'@param df A dataframe with a filename column of extracted template filepaths.
#'@return A modified version of the input df orgnaized by file directory.
#'@import dplyr tidyr
organize_directory_groups <- function(df=NULL){
  return(test = df %>%
    separate(col=filename, into=c("QA Category", "QA Subcategory"), sep="/", 
             extra="drop", remove=FALSE) %>%
    mutate(base=basename(filename),
           `QA Subcategory` = ifelse(`QA Subcategory`==base, NA, `QA Subcategory`),
           base = gsub("_CvT_data_template_articles","",base)) %>%
    separate(col=base, into=c("PMID", "Curator Initials"), sep="_", fill="right", 
             extra="merge") %>%
    mutate(`Curator Initials`=gsub(".xlsx", "", `Curator Initials`),
           filename = basename(filename),
           uploadReady = ifelse(grepl("qa_format_complete", `QA Category`), 1, 0))
    )
}

#'@title Get Clowder Document List
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param df Dataframe to modify with a "PDF in Clowder" field. Must have PMID values to compare to Clowder documents.
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@return Returns a modified version of the input df with "PDF in Clowder" indicator filled in.
#'@import dplyr
check_pdf_loaded_clowder <- function(df=NULL, apiKey=NULL){
  clowderDocs = get_clowder_docList(clowderDir=c("CvT Data Documents"), 
                                    apiKey = apiKey) %>%
    mutate(PMID = gsub(".pdf","",filename) %>%
             gsub("PMID", "", .) %>% 
             as.numeric()) %>%
    select(PMID)
  return(df %>%
    mutate(`PDF in Clowder` = ifelse(PMID %in% clowderDocs$PMID, 1, 0)))
}

#'@title Get Clowder Document List
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param clowderDir The file directory/dataset name on Clowder.
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@return Returns a dataframe with file details of: filename and ClowderID.
#'@import dplyr
get_clowder_docList <- function(clowderDir=NULL, apiKey=NULL){
  baseurl = "https://clowder.edap-cluster.com/api"
  #Get dataset IDs of interest
  dataset_info <- httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets",
                                   apiKey)) %>% httr::content() %>%
    tibble(id = map_chr(.,"id"), name = map_chr(.,"name")) %>%
    select(-1) %>%
    filter(name %in% clowderDir)
  
  #get list of all files already uploaded.
  return(lapply(dataset_info$id, function(x){
    Sys.sleep(0.25)
    tmp = paste0(baseurl,"/datasets/",x,"/listAllFiles",apiKey)
    return(res <- GET(tmp) %>% 
             content() %>%
             tibble(filename = map_chr(.,"filename"), 
                    id = map_chr(.,"id")) %>% 
             select(-1)
    )
  }) %>% jsonlite::rbind_pages())
}


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

get_clowder_file_metadata <- function(fileID = NULL, apiKey = NULL){
  Sys.sleep(0.25)
  md = GET(paste0("https://clowder.edap-cluster.com/api/files/",
                  fileID,
                  "/metadata.jsonld",
                  apiKey)) %>%
    content()
 # return(lapply(md[[1]]$content, function(x){ ifelse(is_null(x), NA, x) }) %>%
 #          rbind.data.frame())
  if(is_empty(md)){
    return(md)
  } else {
    return(lapply(md[[1]]$content, function(x){ ifelse(is_null(x), 
                                                       as.character(NA), x) }))
  }
}

compare_metadata <- function(fileID=NULL, new_metadata=NULL, apiKey=NULL){
  return(!identical(get_clowder_file_metadata(fileID=fileID, apiKey=apiKey), 
                    new_metadata))
}

get_user_credentials <- function(){
  userID <- Sys.getenv("clowder_user_id")
  apiKey <- Sys.getenv("apiKey")# "?key=12345-12345-asdf"
  
  if(interactive()){
    message("Handle interactive session needs")
    if(grepl("*INSERT USER ID HERE*", userID)){
      userID <- readline(prompt="Enter Clowder UserID URL: " )
    } 
    
    #INSERT LOGIC TO CHECK USERID IS CORRECT
    
    if(grepl("*INSERT API KEY HERE*", apiKey)){
      apiKey = paste0("?key=", readline(prompt="Enter Clowder API Key: " ))
    }
    
    if(GET(paste0("https://clowder.edap-cluster.com/api/datasets", apiKey)) %>% 
       status_code() != 200){
      stop("Incorrect apiKey...")
    }
  } else {
    cat("Enter Clowder UserID URL:\n")
    userID <- readLines(file("stdin"), n = 1L)
    cat("Enter Clowder apiKey:\n")
    apiKey <- readLines(file("stdin"), n = 1L)
  }
  
  userID <<- userID
  apiKey <<- apiKey
  #return(list(userID=userID, apiKey=apiKey))
}

reset_environment_file <- function(){
  reset = readline("Reset .Renviron file? (y/n): ")
  if(reset == "y"){
    message("Resetting .Renviron file...")
    if(!file.exists(".Renviron")){
      file.create(".Renviron")
    }
    fileConn<-file(".Renviron")
    writeLines(c('apiKey = "?key=*INSERT API KEY HERE*"',
                 'clowder_user_id="*INSERT USER ID HERE*"'), fileConn)
    close(fileConn)    
  }
  message("Done...")
}
