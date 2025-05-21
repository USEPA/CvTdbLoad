cvtdb_release_comparison_stats <- function(){
  
  if(!dir.exists("output/release/version_comparison")) dir.create("output/release/version_comparison", recursive = TRUE)
  
  cvtdb_list = list(
    v2.0.0 = "output/cvtdb_sqlite/cvt_db_20250515.sqlite",
    v1.1.0 = "output/cvtdb_sqlite/cvt_db_20210607.sqlite",
    v1.0.0 = "output/cvtdb_sqlite/cvt_db_20191219.sqlite"
  )
  
  ## Helper functions to connect SQLite files
  
  #'@description A function to make a connection to the database
  #'@param db Filepath to SQLite file
  #'@import DBI RSQLite
  #'@return Database connection pointer object
  connect_to_db <- function(db){
    if(file.exists(db)){
      return(DBI::dbConnect(RSQLite::SQLite(), db))
    }
    return(NULL)
  }
  
  #'@description Function to query database and receive the results. 
  #'Handles errors/warnings with tryCatch.
  #'@param query A SQL query string to query the database with
  #'@param db Filepath to SQLite file
  #'@import DBI dplyr
  #'@return Dataframe of database query results
  query_db <- function(query=NULL, db){
    if(is.null(query)) return(cat("\nMust provide a query to send"))
    con = connect_to_db(db)
    
    query_result = tryCatch({
      RSQLite::dbGetQuery(con, query)
    },
    error=function(cond){ cat(paste0("\nError message: ", cond)); return(NULL) },
    finally={ RSQLite::dbDisconnect(con) })
    
    return(query_result)
  }
  
  # Loop through versions and com
  cvtdb_release_comparison = lapply(names(cvtdb_list), function(db_v){
    message("Pulling ", db_v, " (", which(db_v == names(cvtdb_list))," of ", length(cvtdb_list),")")
    # Set database name
    cvt.db = cvtdb_list[[db_v]]
    
    # SQL file doesn't exist
    if(!file.exists(cvt.db)) return(NULL)
    
    data.frame(
      `Extraction Documents linked to a Study` = query_db(query = paste0("SELECT count(*) as n FROM documents ",
                                                                         "WHERE id in (SELECT fk_extraction_document_id FROM studies)"),
                                                          db = cvt.db) %>%
        dplyr::pull(n),
      
      `Reference Documents linked to a Study` = query_db(paste0("SELECT count(*) as n FROM documents ",
                                                                "WHERE id in (SELECT fk_reference_document_id FROM studies)"),
                                                         db = cvt.db) %>%
        dplyr::pull(n),
      
      `Unique Chemical DTXSID Entries` = switch(db_v, 
                                                # v1.0.0 did not have a chemicals table
                                                "v1.0.0" = query_db(
                                                  query = paste0("SELECT DISTINCT analyte_dtxsid, test_substance_dtxsid FROM series"),
                                                  db = cvt.db) %>%
                                                  unlist() %>% 
                                                  unname() %>%
                                                  unique() %>%
                                                  length(),
                                                # All other versions with a chemicals table                                      
                                                query_db(
                                                  query = paste0("SELECT count(distinct dsstox_substance_id) as n FROM chemicals ",
                                                                 "WHERE dsstox_substance_id IS NOT NULL AND ",
                                                                 "(id IN (SELECT fk_dosed_chemical_id FROM studies) OR ",
                                                                 "id IN (SELECT fk_analyzed_chemical_id FROM series))"),
                                                  db = cvt.db) %>%
                                                  dplyr::pull(n)
      ),
      
      `Total Studies` = query_db(paste0("SELECT count(*) as n FROM studies ",
                                        "WHERE id in (SELECT fk_study_id FROM series)"),
                                 db = cvt.db) %>%
        dplyr::pull(n),
      
      `Total Dosed Chemicals Mapped to DTXSID` = switch(db_v, 
                                                        # v1.0.0 did not have a chemicals table
                                                        "v1.0.0" = query_db(
                                                          query = paste0("SELECT count(DISTINCT test_substance_dtxsid) as n FROM series"),
                                                          db = cvt.db) %>%
                                                          dplyr::pull(n),
                                                        # All other versions with a chemicals table                                      
                                                        query_db(paste0("SELECT count(distinct dsstox_substance_id) as n FROM chemicals ",
                                                                        "WHERE dsstox_substance_id IS NOT NULL AND id in (SELECT fk_dosed_chemical_id FROM studies ",
                                                                        "WHERE id IN (SELECT DISTINCT fk_study_id FROM series))"),
                                                                 db = cvt.db) %>%
                                                          dplyr::pull(n)
      ),
      
      `Total Administration Routes by Study Count` = switch(db_v, 
                                                            # v1.0.0 did not have an administration_route dictionary table
                                                            "v1.0.0" = query_db(
                                                              query = paste0(
                                                                "SELECT distinct id, administration_route_normalized ",
                                                                "FROM studies ",
                                                                "WHERE id in (SELECT distinct fk_study_id FROM series) ",
                                                                "AND administration_route_normalized IS NOT NULL"
                                                              ),
                                                              db = cvt.db) %>%
                                                              dplyr::count(administration_route_normalized) %>%
                                                              dplyr::arrange(administration_route_normalized),
                                                            # v1.1.0 did not have an administration_route dictionary table
                                                            "v1.1.0" = query_db(
                                                              query = paste0(
                                                                "SELECT distinct id, administration_route_normalized ",
                                                                "FROM studies ",
                                                                "WHERE id in (SELECT distinct fk_study_id FROM series) ",
                                                                "AND administration_route_normalized IS NOT NULL ",
                                                                "AND administration_route_normalized != ''"
                                                              ),
                                                              db = cvt.db) %>%
                                                              dplyr::count(administration_route_normalized) %>%
                                                              dplyr::arrange(administration_route_normalized),
                                                            # All other versions with an administration_route dictionary table                                      
                                                            query_db(
                                                              query = paste0(
                                                                "SELECT distinct a.id, b.administration_route_normalized ",
                                                                "FROM studies a ",
                                                                "LEFT JOIN administration_route_dict b ON a.fk_administration_route_id = b.id ",
                                                                "WHERE a.id in (SELECT distinct fk_study_id FROM series) ",
                                                                "AND b.administration_route_normalized IS NOT NULL"
                                                              ),
                                                              db = cvt.db) %>%
                                                              dplyr::count(administration_route_normalized) %>%
                                                              dplyr::arrange(administration_route_normalized)
      ) %>%
        tidyr::unite(col = "administration_route_normalized_n", administration_route_normalized, n, sep = " - ") %>%
        dplyr::pull(administration_route_normalized_n) %>%
        paste0(collapse = "; "),
      
      `Total Subjects` = query_db(paste0("SELECT count(*) as n FROM subjects ",
                                         "WHERE id in (SELECT fk_subject_id FROM series)"),
                                  db = cvt.db) %>%
        dplyr::pull(n),
      
      `Total Species by Study Count` = query_db(
        query = paste0(
        "SELECT distinct a.fk_study_id, b.species ",
        "FROM series a ",
        "LEFT JOIN subjects b ON a.fk_subject_id = b.id"
      ),
      db = cvt.db) %>%
        dplyr::mutate(species = tolower(species)) %>%
        dplyr::count(species) %>%
        dplyr::arrange(species) %>%
        tidyr::unite(col = "species_n", species, n, sep = " - ") %>%
        dplyr::pull(species_n) %>%
        paste0(collapse = "; "),
      
      `Total Series` = query_db(paste0("SELECT count(*) as n FROM series WHERE id IN ",
                                       "(SELECT distinct fk_series_id FROM conc_time_values) ",
                                       "AND fk_study_id IN (SELECT id FROM studies WHERE fk_extraction_document_id IN (",
                                       "SELECT id FROM documents))"),
                                db = cvt.db) %>%
        dplyr::pull(n),
      
      `Total Analyzed Chemicals Mapped to DTXSID` = switch(db_v, 
                                                           # v1.0.0 did not have a chemicals table
                                                           "v1.0.0" = query_db(
                                                             query = paste0("SELECT count(DISTINCT analyte_dtxsid) as n FROM series"),
                                                             db = cvt.db) %>%
                                                             dplyr::pull(n),
                                                           # All other versions with a chemicals table                                      
                                                           query_db(paste0("SELECT count(distinct dsstox_substance_id) as n FROM chemicals ",
                                                                           "WHERE dsstox_substance_id IS NOT NULL AND id in (SELECT fk_analyzed_chemical_id FROM series)"),
                                                                    db = cvt.db) %>%
                                                             dplyr::pull(n)
      ),
      
      `Total Instances Where the Dosed Chemical is Different from the Analyzed Chemical` = {
        tmp = switch(db_v, 
                     # v1.0.0 did not have a chemicals table
                     "v1.0.0" = query_db(
                       query = paste0("SELECT id as series_id, fk_study_id, ",
                                      "test_substance_dtxsid, analyte_dtxsid ",
                                      "FROM series ",
                                      "WHERE test_substance_dtxsid != analyte_dtxsid"
                       ),
                       db = cvt.db),
                     # All other versions with a chemicals table                                      
                     query_db(
                       paste0(
                         "SELECT ",
                         # "b.id as series_id, b.fk_study_id, ",
                         " DISTINCT b.id as series_id, b.fk_study_id, ",
                         "k.dosed_chem_dtxsid, l.analyzed_chem_dtxsid ",
                         # # Studies table fields
                         # "c.fk_dosed_chemical_id, ",
                         # ## Chemical dictionary fields (dosed chemical information)
                         # "k.dosed_chem_dtxsid, ",
                         # # Series table fields
                         # "b.fk_analyzed_chemical_id, ",
                         # 
                         # ## Chemical dictionary fields (analyzed chemical information)
                         # "l.analyzed_chem_dtxsid ",
                         
                         # Join with series table by series ID
                         "FROM series b ",
                         
                         # Join to studies table by study ID
                         "LEFT JOIN studies c ON b.fk_study_id = c.id ",
                         
                         # Rename chemical fields for dosed vs. analyzed chemical record foreign keys
                         "LEFT JOIN (SELECT id, dsstox_substance_id as dosed_chem_dtxsid ",
                         "FROM chemicals) as k ON c.fk_dosed_chemical_id = k.id ",
                         "LEFT JOIN (SELECT id, dsstox_substance_id as analyzed_chem_dtxsid ",
                         "FROM chemicals) as l ON b.fk_analyzed_chemical_id = l.id ",
                         "WHERE k.dosed_chem_dtxsid != l.analyzed_chem_dtxsid"
                       ),
                       db = cvt.db
                     )
        )
        # Output as string to parse later
        paste0(c(tmp %>%
                   dplyr::select(dplyr::contains("dtxsid")) %>%
                   dplyr::distinct() %>%
                   nrow(), 
                 tmp %>%
                   dplyr::select(-series_id) %>%
                   dplyr::distinct() %>%
                   nrow(),
                 nrow(tmp)),
               collapse = ";"
        )
      },
      
      `Total Concentration-Time Values with Normalized Units` = query_db(paste0("SELECT count(*) as n FROM conc_time_values ",
                                                                                "WHERE fk_series_id in (SELECT id FROM series) ",
                                                                                "AND conc IS NOT NULL"),
                                                                         db = cvt.db) %>%
        dplyr::pull(n)
    ) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "stat",
                          values_to = "count",
                          values_transform = as.character) %>%
      dplyr::mutate(version = db_v)
  }) %>%
    dplyr::bind_rows() %>%
    # Reformat verison
    dplyr::mutate(version = version %>%
                    factor(),
                  stat = stat %>%
                    gsub(".", " ", ., fixed = TRUE)
    )
  
  # Save RData
  message("Saving stats to: ", file.path("output/release/version_comparison",
                                         paste0(names(cvtdb_list)[1], ".RData")))
  save(cvtdb_release_comparison,
       file = file.path("output/release/version_comparison",
                        paste0(names(cvtdb_list)[1], ".RData"))
  )
  
}