cvtdb_release_comparison_stats <- function(){
  
  if(!dir.exists("output/release/version_comparison")) dir.create("output/release/version_comparison", recursive = TRUE)
  
  cvtdb_list = list(
    v2.0.0 = "output/cvtdb_sqlite/res_db_20250707.sqlite",
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
      `Extraction Documents Linked to a Study` = query_db(query = paste0("SELECT count(*) as n FROM documents ",
                                                                         "WHERE id in (SELECT fk_extraction_document_id FROM studies)"),
                                                          db = cvt.db) %>%
        dplyr::pull(n),
      
      `Reference Documents Linked to a Study` = query_db(paste0("SELECT count(*) as n FROM documents ",
                                                                "WHERE id in (SELECT fk_reference_document_id FROM studies)"),
                                                         db = cvt.db) %>%
        dplyr::pull(n),
      
      `Unique Chemical DTXSID Entries Linked to Study or Series` = switch(db_v, 
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
      
      `Total Studies Linked to Series` = query_db(paste0("SELECT count(*) as n FROM studies ",
                                                         "WHERE id in (SELECT fk_study_id FROM series)"),
                                                  db = cvt.db) %>%
        dplyr::pull(n),
      
      `Total Dosed Chemicals Mapped to DTXSID` = switch(db_v, 
                                                        # v1.0.0 and v1.1.0 did not have a chemicals table that worked in the same way as v2
                                                        "v1.0.0" = paste0(
                                                          # Get number of unique dose chemicals (name-casrn pairs) overall
                                                          query_db(
                                                            query = paste0("SELECT distinct a.test_substance_name, b.test_substance_casrn ",
                                                                           "FROM studies a ",
                                                                           "LEFT JOIN series b on a.id = b.fk_study_id ",
                                                                           "WHERE a.id in (SELECT fk_study_id FROM series)"),
                                                            db = cvt.db) %>%
                                                            nrow(),
                                                          "; ",
                                                          # Get number of chemicals mapped to DTXSID
                                                          query_db(
                                                            query = paste0("SELECT distinct a.test_substance_name, b.test_substance_casrn ",
                                                                           "FROM studies a ",
                                                                           "LEFT JOIN series b on a.id = b.fk_study_id ",
                                                                           "WHERE b.test_substance_dtxsid IS NOT NULL AND ",
                                                                           "a.id in (SELECT fk_study_id FROM series)"),
                                                            db = cvt.db
                                                          ) %>%
                                                            nrow(),
                                                          "; ",
                                                          # Get unique DTXSID assignments
                                                          query_db(
                                                            query = paste0("SELECT count(DISTINCT test_substance_dtxsid) as n FROM series"),
                                                            db = cvt.db) %>%
                                                            dplyr::pull(n)
                                                        ),
                                                        "v1.1.0" = paste0(
                                                          # Get number of unique dose chemicals (name-casrn pairs) overall
                                                          query_db(
                                                            query = paste0("SELECT distinct a.test_substance_name_original, b.test_substance_casrn ",
                                                                           "FROM studies a ",
                                                                           "LEFT JOIN series b on a.id = b.fk_study_id ",
                                                                           "WHERE a.id in (SELECT fk_study_id FROM series)"),
                                                            db = cvt.db
                                                          ) %>%
                                                            nrow(),
                                                          "; ",
                                                          # Get number of chemicals mapped to DTXSID
                                                          query_db(
                                                            query = paste0("SELECT distinct a.test_substance_name_original, b.test_substance_casrn ",
                                                                           "FROM studies a ",
                                                                           "LEFT JOIN series b on a.id = b.fk_study_id ",
                                                                           "WHERE fk_dosed_chemical_id in (SELECT id FROM chemicals WHERE dsstox_substance_id IS NOT NULL) ",
                                                                           "AND a.id in (SELECT fk_study_id FROM series)"),
                                                            db = cvt.db
                                                          ) %>%
                                                            nrow(),
                                                          "; ",
                                                          # Get unique DTXSID assignments
                                                          query_db(
                                                            query = paste0("SELECT distinct dsstox_substance_id FROM chemicals ",
                                                                           "WHERE id in (SELECT fk_dosed_chemical_id FROM studies ",
                                                                           "WHERE id in (SELECT fk_study_id FROM series)) and dsstox_substance_id IS NOT NULL"),
                                                            db = cvt.db) %>%
                                                            nrow()
                                                        ),
                                                        "v2.0.0" = 
                                                          paste0(
                                                            # Get total dosed chemicals
                                                            query_db(
                                                              query = paste0("SELECT * FROM chemicals WHERE id in (SELECT fk_dosed_chemical_id FROM studies ",
                                                                             "WHERE id in (SELECT DISTINCT fk_study_id FROM series))"),
                                                              db = cvt.db
                                                            ) %>%
                                                              nrow(),
                                                            "; ",
                                                            # Get mapped chemicals
                                                            query_db(
                                                              query = paste0(
                                                                "select count(distinct fk_dosed_chemical_id) as n_chem_id ",
                                                                "from studies ",
                                                                "where fk_dosed_chemical_id in ",
                                                                "(select id from chemicals where dsstox_substance_id is not null) and ",
                                                                "id in (select fk_study_id from series)"
                                                              ),
                                                              db = cvt.db) %>%
                                                              dplyr::pull(n_chem_id),
                                                            "; ", 
                                                            # Get unique DTXSID values
                                                            query_db(query = paste0("SELECT count(distinct dsstox_substance_id) as n_dtxsid FROM chemicals ",
                                                                                    "WHERE dsstox_substance_id IS NOT NULL AND id in (SELECT fk_dosed_chemical_id FROM studies ",
                                                                                    "WHERE id IN (SELECT DISTINCT fk_study_id FROM series))"),
                                                                     db = cvt.db) %>%
                                                              dplyr::pull(n_dtxsid)
                                                          )
                                                        
      ),
      
      `Total Study Count by Administration Routes Linked to Series` = switch(db_v, 
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
        dplyr::arrange(administration_route_normalized) %>%
        dplyr::mutate(administration_route_normalized = paste0("**", administration_route_normalized, "**")) %>%
        tidyr::unite(col = "administration_route_normalized_n", administration_route_normalized, n, sep = " (") %>%
        dplyr::mutate(administration_route_normalized_n = paste0(administration_route_normalized_n, ")")) %>%
        # tidyr::unite(col = "administration_route_normalized_n", administration_route_normalized, n, sep = " - ") %>%
        dplyr::pull(administration_route_normalized_n) %>%
        paste0(collapse = ", "),
      
      `Total Subjects` = query_db(paste0("SELECT count(*) as n FROM subjects ",
                                         "WHERE id in (SELECT fk_subject_id FROM series)"),
                                  db = cvt.db) %>%
        dplyr::pull(n),
      
      `Total Study Count by Species` = query_db(
        query = paste0(
          "SELECT distinct a.fk_study_id, b.species ",
          "FROM series a ",
          "LEFT JOIN subjects b ON a.fk_subject_id = b.id"
        ),
        db = cvt.db) %>%
        dplyr::count(species) %>%
        dplyr::arrange(species) %>%
        dplyr::mutate(species = paste0("**", tolower(species), "**")) %>%
        tidyr::unite(col = "species_n", species, n, sep = " (") %>%
        dplyr::mutate(species_n = paste0(species_n, ")")) %>%
        dplyr::pull(species_n) %>%
        paste0(collapse = ", "),
      
      `Total Series Linked to Conc_Time_Values` = query_db(paste0("SELECT count(*) as n FROM series WHERE id IN ",
                                                                  "(SELECT distinct fk_series_id FROM conc_time_values) ",
                                                                  "AND fk_study_id IN (SELECT id FROM studies WHERE fk_extraction_document_id IN (",
                                                                  "SELECT id FROM documents))"),
                                                           db = cvt.db) %>%
        dplyr::pull(n),
      
      `Total Analyzed Chemicals Mapped to DTXSID` = switch(db_v, 
                                                           # v1.0.0 and v1.1.0 did not have a chemicals table like v2.0.0
                                                           "v1.0.0" = paste0(
                                                             # Get number of unique analyte chemicals (name-casrn pairs) overall
                                                             query_db(
                                                               query = "SELECT distinct analyte_name, analyte_casrn FROM series",
                                                               db = cvt.db
                                                             ) %>%
                                                               nrow(),
                                                             "; ",
                                                             # Get total number of mapped chemicals
                                                             query_db(
                                                               query = "SELECT distinct analyte_name, analyte_casrn FROM series where analyte_dtxsid IS NOT NULL",
                                                               db = cvt.db
                                                             ) %>%
                                                               nrow(),
                                                             "; ",
                                                             # Get unique DTXSID assignments
                                                             query_db(
                                                               query = paste0("SELECT count(DISTINCT analyte_dtxsid) as n FROM series"),
                                                               db = cvt.db) %>%
                                                               dplyr::pull(n) 
                                                           ),
                                                           "v1.1.0" = paste0(
                                                             # Get number of unique analyte chemicals (name-casrn pairs) overall
                                                             query_db(
                                                               query = "SELECT distinct analyte_name_original, analyte_casrn FROM series",
                                                               db = cvt.db
                                                             ) %>%
                                                               nrow(),
                                                             "; ",
                                                             # Get total number of mapped chemicals
                                                             query_db(
                                                               query = paste0("SELECT distinct analyte_name_original, analyte_casrn ",
                                                                              "FROM series WHERE fk_analyzed_chemical_id IN (SELECT id FROM chemicals WHERE dsstox_substance_id IS NOT NULL)"),
                                                               db = cvt.db
                                                             ) %>%
                                                               nrow(),
                                                             "; ",
                                                             # Get unique DTXSID assignments
                                                             query_db(
                                                               query = paste0("SELECT distinct dsstox_substance_id FROM chemicals ",
                                                                              "WHERE id in (SELECT fk_analyzed_chemical_id FROM series) AND dsstox_substance_id IS NOT NULL"),
                                                               db = cvt.db) %>%
                                                               nrow()
                                                           ),
                                                           "v2.0.0" = paste0(
                                                             # Get number of unique analyte chemicals (unique chemical ID values)
                                                             query_db(
                                                               query = paste0("SELECT * FROM chemicals WHERE id in (SELECT fk_analyzed_chemical_id FROM series)"),
                                                               db = cvt.db
                                                             ) %>%
                                                               nrow(),
                                                             "; ",
                                                             # Get total number of mapped chemicals
                                                             query_db(
                                                               query = paste0(
                                                                 "select count(distinct fk_analyzed_chemical_id) as n_chem_id ",
                                                                 "from series ",
                                                                 "where fk_analyzed_chemical_id in ",
                                                                 "(select id from chemicals where dsstox_substance_id is not null)"
                                                               ),
                                                               db = cvt.db) %>%
                                                               dplyr::pull(n_chem_id),
                                                             "; ", 
                                                             # Get unique DTXSID assignments
                                                             query_db(paste0("SELECT count(distinct dsstox_substance_id) as n FROM chemicals ",
                                                                             "WHERE dsstox_substance_id IS NOT NULL AND id in (SELECT fk_analyzed_chemical_id FROM series)"),
                                                                      db = cvt.db) %>%
                                                               dplyr::pull(n)
                                                           )
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
                   dplyr::select(fk_study_id) %>%
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
        dplyr::pull(n),
      `Total TK Parameters by Extraction Document Count` = switch(
        db_v,
        "v1.0.0" = query_db(
          paste0("SELECT distinct a.parameter_name, c.fk_extraction_document_id  ",
                 "FROM tk_parameters a ",
                 "LEFT JOIN series b ON a.fk_series_id = b.id ",
                 "LEFT JOIN studies c ON b.fk_study_id = c.id"),
          cvt.db
        ),
        # v1.1.0 onward had the fk_study_id in the tk_parameters table
        query_db(
          query = paste0(
            "SELECT distinct a.parameter_name, b.fk_extraction_document_id  ",
            "FROM tk_parameters a ",
            "LEFT JOIN studies b ON a.fk_study_id = b.id ",
            "UNION ",
            "SELECT distinct a.parameter_name, c.fk_extraction_document_id  ",
            "FROM tk_parameters a ",
            "LEFT JOIN series b ON a.fk_series_id = b.id ",
            "LEFT JOIN studies c ON b.fk_study_id = c.id"
          ), db = cvt.db)
      ) %>%
        dplyr::filter(!is.na(fk_extraction_document_id)) %>%
        dplyr::distinct() %>%
        dplyr::count(parameter_name) %>%
        dplyr::arrange(parameter_name) %>%
        dplyr::mutate(parameter_name = paste0("**", parameter_name, "**")) %>%
        tidyr::unite(col = "parameter_name_n", parameter_name, n, sep = " (") %>%
        dplyr::mutate(parameter_name_n = paste0(parameter_name_n, ")")) %>%
        dplyr::pull(parameter_name_n) %>%
        paste0(collapse = ", ")
      
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
