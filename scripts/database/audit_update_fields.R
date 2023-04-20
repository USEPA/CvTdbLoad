#--------------------------------------------------------------------------------------
#' @description Function to add/modify/delete select audit columns to source table
#' @param s_tbl Source table name to apply changes to
#' @param field_lsit List of current field names in source table
#' @param db_schema the schema of the database
#--------------------------------------------------------------------------------------
audit.update.fields <- function(s_tbl, field_list, db_schema){
  # Update rec_create_dt to always update timestamp
  if("rec_create_dt" %in% field_list){
    query_cvt(paste0("ALTER TABLE ",db_schema,".", s_tbl," ALTER COLUMN rec_create_dt TYPE timestamp;"))
    query_cvt(paste0("ALTER TABLE ",db_schema,".", s_tbl," ALTER COLUMN rec_create_dt SET DEFAULT now();"))
  } else {
    # Add rec_create_dt if not present
    query_cvt(query = paste0("ALTER TABLE ",db_schema, ".", s_tbl,
                    " ADD COLUMN rec_create_dt timestamp default now();"))
  }
  
  # Drop rec_update_dt
  if("rec_update_dt" %in% field_list){
    query_cvt(query = paste0("ALTER TABLE ",db_schema, ".", s_tbl," DROP COLUMN rec_update_dt;"))
  }
  
  # Drop updated_by
  if("updated_by" %in% field_list){
    query_cvt(query = paste0("ALTER TABLE ",db_schema, ".", s_tbl," DROP COLUMN updated_by;"))
    
  }
  
  update_list = c("qc_status", "qc_flags", "qc_notes", "version") %>%
    .[!. %in% field_list]
  for(u in update_list){
    if(!u %in% field_list){
      query = switch(u,
                     # Add version
                     version = paste0("ALTER TABLE ",db_schema, ".", s_tbl,
                                      " ADD version integer DEFAULT 1 NOT NULL;"),
                     # Add qc_status
                     qc_status = paste0("ALTER TABLE ",db_schema, ".", s_tbl,
                                        " ADD qc_status varchar(45) DEFAULT NULL;"),
                     # Add qc_flags
                     qc_flags = paste0("ALTER TABLE ",db_schema, ".", s_tbl,
                                       " ADD qc_flags text DEFAULT NULL;"),
                     # Add qc_notes
                     qc_notes = paste0("ALTER TABLE ",db_schema, ".", s_tbl,
                                       " ADD qc_notes text DEFAULT NULL;"),
                     { NULL }
      )
      query_cvt(query=query)
    }
  }
}
