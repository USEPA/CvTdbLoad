#Script to match/update foreign key assignments in CvTdb
#Created by: Jonathan Taylor Wall
#Created Date: 2022-02-17
#Load packages
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
invisible(sapply(file_source, source,.GlobalEnv))

#Params
update_all = FALSE

tbl_info = list(conc = list(select_list = c("id", "conc_medium_original"),
                            mutate_list = c("conc_medium_original"),
                            dict = "cvt.conc_medium_dict",
                            join_list = c("conc_medium_original"),
                            id_col = "fk_conc_medium_id",
                            update_tbl="cvt.series"),
                administration_route = list(select_list = c("id", "administration_route_original"),
                                            mutate_list = c("administration_route_original"),
                                            dict = "cvt.administration_route_dict",
                                            join_list = c("administration_route_original"),
                                            id_col = "fk_administration_route",
                                            update_tbl = "cvt.studies")
                )

message("Starting processing...", Sys.time())
for(type in names(tbl_info)){
  message("Processing ", type, "...", Sys.time())
  #Check required fields
  if(!tbl_info[[type]][c("select_list", "mutate_list", "dict", "join_list", "id_col", "update_tbl")] %>% 
     purrr::compact() %>% length()){
    message("...missing required field in tbl_info for type '", type,"'...skipping type")
    next
  }
  
  #Pull data to update
  if(update_all){#Update all records
    input = db_query_cvt(paste0("SELECT ", toString(tbl_info[[type]]$select_list), " FROM ", tbl_info[[type]]$update_tbl))
  } else { #Only update what is NULL
    input = db_query_cvt(paste0("SELECT ", toString(tbl_info[[type]]$select_list), 
                             " FROM ", tbl_info[[type]]$update_tbl, " WHERE ", tbl_info[[type]]$id_col, " IS NULL"))
  }
  
  if(!nrow(input)){
    message("...no data returned for input...skipping...")
    next
  }
  #Normalize input
  input = input %>%
    mutate(across(tbl_info[[type]]$mutate_list, ~tolower(trimws(.))))
  #Get dictionary to match to and normalize
  dict = db_query_cvt(paste0("SELECT ", toString(tbl_info[[type]]$select_list), 
                     " FROM ", tbl_info[[type]]$dict)) %>%
    dplyr::rename(!!tbl_info[[type]]$id_col := id) %>%
    mutate(across(tbl_info[[type]]$mutate_list, ~tolower(trimws(.))))
  #Match by joining
  output = input %>%
    left_join(dict, by=tbl_info[[type]]$join_list)
  
  #Check which didn't match - need to update dictionary
  #unique(output$conc_medium_original[is.na(output$fk_conc_medium_id)])
  message("...unmatched records: ", length(unique(output[[tbl_info[[type]]$mutate_list]][is.na(output[[tbl_info[[type]]$id_col]])])))
  
  #Push updates
  con = db_connect_to_CvT()
  dbWriteTable(con, value = output, name=c("cvt", "temp_tbl"), overwrite=TRUE, row.names=FALSE)  
  dbDisconnect(con)
  
  query = paste0("UPDATE ",tbl_info[[type]]$update_tbl," h SET ",tbl_info[[type]]$id_col," = m.",tbl_info[[type]]$id_col,
                 " FROM cvt.temp_tbl m",
                 " WHERE h.id = m.id")  
  #Make update (only uncomment when ready to use)
  #db_query_cvt(query=query)
  db_query_cvt("DROP TABLE cvt.temp_tbl")
  
}
message("Done...", Sys.time())

