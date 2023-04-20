#--------------------------------------------------------------------------------------
#' Create audit table and add BEFORE UPDATE audit triggers to source_* tables
#'
#' @param db_schema the schema of the database
#' @export
#--------------------------------------------------------------------------------------
init.audit.table <- function(db_schema){
  # List of ID fields not to be added to JSON of audit
  id_list = c("id", "qc_status", "qc_flags", "qc_notes", "version", "rec_create_dt", "created_by")
  # Load SQL file with audit table and trigger creation queries
  audit_sql = parse_sql_file("input/audit_sql/audit_init.sql") %T>%
    { names(.) <- c("create_audit", "bu_audit_trigger", "bu_audit_trigger_function", 
                    "drop_bu_audit_trigger", "drop_bu_audit_trigger_function",
                    "bu_source_trigger", "bu_source_trigger_function", 
                    "drop_bu_source_trigger", "drop_bu_source_trigger_function") }
  
  # Create audit table
  query_cvt(query=audit_sql$create_audit)
  
  # Get list of source tables to add triggers
  tblList = query_cvt(query=paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = '",db_schema,"'")) %>%
    unlist() %>% 
    unname() %>%
    # Ignore those like audit, chemicals, dictionary, tk_parameters_* linker tables
    .[!grepl("audit|dict|chemical|parameters_", .)]
  
  # Drop increment trigger function
  query_cvt(query=audit_sql$drop_bu_source_trigger_function)
  # Apply increment trigger functions to database
  query_cvt(query=audit_sql$bu_source_trigger_function)
  # Check if it was applied appropriately
  if(!query_cvt(query = "select exists(select * from pg_proc where proname = 'increment_version_set_user');")$exists){
    stop("Issue creating increment_version_set_user function...")
  }
  
  # Loop through each table, get fields for JSON, reparse SQL, run Statement
  for(s_tbl in tblList){
    cat("Applying audit trigger to ", s_tbl, "\n")
    field_list = query_cvt(query=paste0("SELECT * FROM ",db_schema,".", s_tbl, " LIMIT 1")) %>%
      names()
    # Update audit fields as needed
    audit.update.fields(s_tbl=s_tbl, field_list=field_list, db_schema=db_schema)
    # Get updated field_list after audit updates
    field_list = query_cvt(query=paste0("SELECT * FROM ",db_schema,".", s_tbl, " LIMIT 1")) %>%
      names()
    # Remove ID fields (don't add to JSON record field of audit table)
    field_list = field_list[!field_list %in% id_list]
    # Parse custom trigger for source table and fields
    
    # BEFORE UPDATE TRIGGER
    src_bu_audit_trigger = audit_sql$bu_audit_trigger %>%
      # Insert source table name
      gsub("cvt_table", s_tbl, .)
    
    # BEFORE UPDATE TRIGGER FUNCTION
    src_bu_audit_trigger_function = audit_sql$bu_audit_trigger_function %>%
      # Insert source table name
      gsub("cvt_table", s_tbl, .) %>%
      # Format JSON
      gsub("JSON_OBJECT\\(\\)", paste0("JSON_OBJECT(ARRAY[",
                                       paste0("'", field_list, "', OLD.", field_list,
                                              collapse=", "),
                                       "])"),
           .) %>%
      paste0(#"DELIMITER // \n",
        ., "\nEND;")#// DELIMITER;")
    
    # BEFORE UPDATE TRIGGER 2: increment version enforcement
    src_bu_source_trigger = audit_sql$bu_source_trigger %>%
      # Insert source table name
      gsub("cvt_table|cvt_table_update", s_tbl, .) %>%
      # Format JSON
      paste0(#"DELIMITER // \n",
        ., "\nEND;")#// DELIMITER;")
    
    # Drop trigger if exists already
    query_cvt(query=audit_sql$drop_bu_audit_trigger %>%
               gsub("cvt_table", s_tbl, .))
    
    # Drop trigger function if exists already
    query_cvt(query=audit_sql$drop_bu_audit_trigger_function %>%
               gsub("cvt_table", s_tbl, .))
    
    # Apply custom audit function to database
    query_cvt(query=src_bu_audit_trigger_function)
    if(!query_cvt(query = paste0("select exists(select * from pg_proc where proname = '",s_tbl,"_audit_tbl_bu');"))$exists){
      stop(paste0("Issue creating ",s_tbl,"_audit_tbl_bu function..."))
    }
    # Apply trigger to table
    query_cvt(query=src_bu_audit_trigger)
    query_cvt(query=src_bu_source_trigger)
  }
}

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
