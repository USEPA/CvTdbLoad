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
