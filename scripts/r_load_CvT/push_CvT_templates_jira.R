# Script to push CvT Extracted data to CvT from Clowder
# Created by: Jonathan Taylor Wall
# Created Date: 2023-11-28

# Pull full list of Clowder folders to help with data provenance labeling
c_folders_list <- clowder_get_dataset_folders(dsID, baseurl, apiKey) %>%
  dplyr::mutate(foldername = foldername %>%
                  gsub("^\\/", "", .)) %>%
  tidyr::separate(foldername, into=c("curation_set_tag", "jira_ticket"), sep="/", 
                  extra="merge", fill="right")
# Pull full list of Clowder files in dataset with "normalized"  stem
c_files_list <- clowder_get_dataset_files(dsID, baseurl, apiKey) %>%
  dplyr::filter(grepl("_normalized.xlsx", filename)) %>%
  dplyr::rename(jira_ticket=folders.name) %>%
  dplyr::left_join(c_folders_list,
                   by="jira_ticket")

