# Script to split CvT units from numeric fields as needed
# Created by: Jonathan Taylor Wall
# Created Date: 2023-02-7

#'@title pp_split_units
#'@description Postprocessing function to split units from numeric fields as needed
#'@param schema_name PostgreSQL schema to query
#'
pp_split_units <- function(schema_name){
  # Pull tables of interest  
  tbl_list = query_cvt(paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = '",
                              schema_name,"' AND table_type = 'BASE TABLE'")) %>%
    # Filter out dictionary, audit tables, etc.
    filter(!grepl("dict|chemical|audit|tk_param", table_name)) %>%
    .[[1]]
  
  # Loop through each table of interest
  for(tbl_n in tbl_list){
    message("Processing table: ", tbl_n)
    # Get potential columns of interest based on "units" fields
    u_fields = query_cvt(paste0("SELECT * FROM ", schema_name, ".", tbl_n, " LIMIT 1")) %>%
      names() %T>% {
        # Cache full table names to filter against
        tbl_n_names <<- .
      } %>%
      # Filter to units columns
      .[grepl("units", .)] %>%
      # Prepare vector of potential fields to pull
      c(., gsub("_units", "", .))
    
    # Remove fields without a numeric - unit field pair
    if(any(!u_fields %in% tbl_n_names)){
      u_fields = u_fields %>%
        .[!grepl(paste0(.[!. %in% tbl_n_names], collapse="|"), .)]
    }
    
    # Remove intermediate not needed
    rm(tbl_n_names)
  
    if(!length(u_fields)) {
      message("...No 'units' fields found in ", tbl_n, "...skipping...")
      next
    }
      
    # Select fields of interest 
    tmp = query_cvt(paste0("SELECT id, ", u_fields %>%
                             sort() %>%
                             toString(), 
                           " FROM ", schema_name, ".", tbl_n))
    
    message("...Found records to review: ", nrow(tmp))
  }
  
}