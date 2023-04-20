#Helper functions to interact with the CvT database and Clowder
#'@description A function to create a connection to the CvT database
#'@import DBI
#'@return A database connection object
db_connect_to_CvT <- function(){
  # dbConnect(RSQLite::SQLite(), 
  #           "L:\\Lab\\HEM\\T_Wall_Projects_FY20\\CvT Database\\input\\sql dump\\CvTdb_20210825.sqlite") %>%
  #   return()#"CvTdb_20210408.sqlite"))
  dbConnect(RPostgreSQL::PostgreSQL(), 
            user = Sys.getenv("user"), 
            password = Sys.getenv("pass"), #
            host = Sys.getenv("host"), #
            dbname = Sys.getenv("dbname")) %>%
    return()
}

#'@description A helper function to query cvt and receive the results. 
#'Handles errors/warnings with tryCatch.
#'@param query A SQL query string to query the database with
#'@import DBI dplyr magrittr
#'@return Dataframe of database query results
db_query_cvt <- function(query=NULL){
  if(is.null(query)) return(message("...Must provide a query to send"))
  con = db_connect_to_CvT()
  query_result = tryCatch({
    dbGetQuery(con, query)
    #dbSendQuery(con, query) %T>% #run query
    #{ dbFetch(.) ->> tmp } %>% #save intermediate variable, critical tee-operator
    #  dbClearResult() #clear result
    #tmp #return query results
  },
  error=function(cond){ message("...Error message: ", cond); return(NA) },
  warning=function(cond){ message("...Warning message: ", cond); return(NULL) },
  finally={ dbDisconnect(con) })
  return(query_result)
}

#' @description Function to write empty tables for CvTdb SQLite file from template docs.
db_initialize_CvTdb <- function(){
  db_push_tbl_to_db(dat=data.frame(id=as.numeric(),
                                pmid=as.character(),
                                other_study_identifier=as.character(),
                                doi=as.character(),
                                first_author=as.character(),
                                year=as.character(),
                                title=as.character(),
                                url=as.character(),
                                curator_comment=as.character()),
                 tblName="Documents",
                 overwrite=TRUE,
                 fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                pmid="varchar(25)",
                                other_study_identifier="varchar(100)",
                                doi="varchar(100)",
                                first_author="varchar(100)",
                                year="varchar(20)",
                                title="varchar(250)",
                                url="varchar(200)",
                                curator_comment="varchar(250)"))
  
  db_push_tbl_to_db(dat = data.frame(id=as.numeric(),
                                  fk_extraction_document_id=as.character(),
                                  test_substance_name=as.character(),
                                  dose_level=as.character(),
                                  dose_level_units=as.character(),
                                  administration_route=as.character(),
                                  dose_duration=as.character(),
                                  dose_frequency=as.character(),
                                  dose_vehicle=as.character(),
                                  dose_volume=as.character(),
                                  fasting_period=as.character(),
                                  author_comment=as.character(),
                                  curator_comment=as.character(),
                                  dermal_dose_vehicle=as.character(),
                                  dermal_dose_vehicle_pH=as.character(),
                                  dermal_applied_area=as.character(),
                                  dermal_applied_area_units=as.character(),
                                  aerosol_particle_diameter_mean=as.character(),
                                  aerosol_particle_diameter_gsd=as.character(),
                                  aerosol_particle_diameter_units=as.character(),
                                  aerosol_particle_density=as.character(),
                                  aerosol_particle_density_units=as.character()),
                 tblName="Studies",
                 overwrite=TRUE,
                 fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                fk_extraction_document_id="varchar(25)",
                                test_substance_name="varchar(100)",
                                dose_level="varchar(100)",
                                dose_level_units="varchar(100)",
                                administration_route="varchar(100)",
                                dose_duration="varchar(100)",
                                dose_frequency="varchar(100)",
                                dose_vehicle="varchar(100)",
                                dose_volume="varchar(100)",
                                fasting_period="varchar(100)",
                                author_comment="varchar(250)",
                                curator_comment="varchar(100)",
                                dermal_dose_vehicle="varchar(100)",
                                dermal_dose_vehicle_pH="varchar(100)",
                                dermal_applied_area="varchar(100)",
                                dermal_applied_area_units="varchar(100)",
                                aerosol_particle_diameter_mean="varchar(100)",
                                aerosol_particle_diameter_gsd="varchar(100)",
                                aerosol_particle_diameter_units="varchar(100)",
                                aerosol_particle_density="varchar(100)",
                                aerosol_particle_density_units="varchar(100)")
  )
  
  db_push_tbl_to_db(dat= data.frame(id=as.numeric(),
                                 species=as.character(),
                                 subtype=as.character(),
                                 sex=as.character(),	
                                 age=as.character(),
                                 age_category=as.character(),
                                 height=as.character(),
                                 weight=as.character(),
                                 curator_comment=as.character()),
                 tblName="Subjects",
                 overwrite=TRUE,
                 fieldTypes = c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                                species="varchar(100)",
                                subtype="varchar(100)",
                                sex="varchar(100)",	
                                age="varchar(100)",
                                age_category="varchar(100)",
                                height="varchar(100)",
                                weight="varchar(100)",
                                curator_comment="varchar(100)"))
  
  db_push_tbl_to_db(dat=data.frame(id=as.numeric(),
                                analyte_name=as.character(),
                                figure_name=as.character(),
                                figure_type=as.character(),
                                figure_series_identifier=as.character(),
                                x_min=as.character(),
                                x_max=as.character(),
                                y_min=as.character(),
                                y_max=as.character(),
                                time_units=as.character(),
                                conc_units=as.character(),
                                log_conc_units=as.character(),
                                loq=as.character(),
                                loq_units=as.character(),
                                lod=as.character(),
                                lod_units=as.character(),
                                analytical_method_detail=as.character(),
                                radiolabeled=as.character(),
                                fk_study_id=as.numeric(),
                                fk_subject_id=as.numeric(),
                                n_subjects_in_series=as.character(),
                                conc_medium=as.character(),
                                curator_comment=as.character()),
                 tblName="Series",
                 overwrite=TRUE,
                 fieldTypes =c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                               analyte_name="varchar(100)",
                               figure_type="varchar(100)",
                               figure_name="varchar(100)",
                               figure_series_identifier="varchar(100)",
                               x_min="varchar(50)",
                               x_max="varchar(50)",
                               y_min="varchar(50)",
                               y_max="varchar(50)",
                               time_units="varchar(100)",
                               conc_units="varchar(100)",
                               log_conc_units="varchar(100)",
                               loq="varchar(100)",
                               loq_units="varchar(100)",
                               lod="varchar(100)",
                               lod_units="varchar(100)",
                               analytical_method_detail="varchar(100)",
                               radiolabeled="varchar(100)",
                               fk_study_id="INTEGER",
                               fk_subject_id="INTEGER",
                               n_subjects_in_series="varchar(100)",
                               conc_medium="varchar(100)",
                               curator_comment="varchar(100)"))
  
  db_push_tbl_to_db(dat=data.frame(id=as.numeric(),
                                fk_series_id=as.numeric(),
                                time=as.character(),
                                conc=as.character(),
                                conc_sd=as.character(),
                                conc_lower_bound=as.character(),
                                conc_upper_bound=as.character(),
                                curator_comment=as.character()),
                 tblName="Conc_Time_Values",
                 overwrite=TRUE,
                 fieldTypes=c(id="INTEGER PRIMARY KEY AUTOINCREMENT",
                              fk_series_id="INTEGER",
                              time="varchar(100)",
                              conc="varchar(100)",
                              conc_sd="varchar(100)",
                              conc_lower_bound="varchar(100)",
                              conc_upper_bound="varchar(100)",
                              curator_comment="varchar(100)"))
  
}

#' @description Function to write empty tables for CvTdb SQLite file from template docs.
db_initialize_CvTdb_from_RDat <- function(file=NULL){
  load(file)
  for(t in names(cvt_dat)){
    db_push_tbl_to_db(dat=cvt_dat[[t]],
                   tblName=t,
                   overwrite=TRUE,
                   fieldTypes = NULL)  
  }
}

db_push_tbl_to_db <- function(dat=NULL, tblName=NULL, fieldTypes=NULL, overwrite=FALSE,
                           customSQL=NULL, append=FALSE){
  if(is.null(dat)) stop("...Error: User must provide data to write to database")
  if(is.null(tblName)) stop("...Error: User must provide a name for the database table")
  
  con = db_connect_to_CvT()
  
  out <- tryCatch({
    message("...Trying to write, '", tblName, "' to CvTdb")
    dbWriteTable(con, value=dat, name=tblName, overwrite=overwrite,
                 field.types=fieldTypes, row.names=FALSE, append=append)
    if(!is.null(customSQL)) { dbSendQuery(con, customSQL) } #Send custom SQL statement
  },
  error=function(cond) { message("...Error message: ", cond); return(NA) },
  warning=function(cond) { message("...Warning message: ", cond); return(NULL) },
  finally={ dbDisconnect(con)
  })
}

#'@title Get Clowder Document List
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@param clowder_dataset A character string for the dataset name
#'@return Returns a dataframe with file details of: file size, date_created, file type, file id, and filename
#'@import dplyr
#'@export
clowder_get_docList <- function(apiKey, clowder_dataset){
  #baseurl <- "https://clowder.ncsa.illinois.edu/clowder/api"
  #get all datasets the user can view
  alldatasets <- httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets",apiKey)) %>% httr::content()
  #look at the first (and only dataset)
  df_id = lapply(alldatasets, function(df){
    return(df$`id`[df$name == clowder_dataset])
  }) %>% purrr::compact() %>% unlist()
  #testdataset <- alldatasets[[2]]$`id`[alldatasets[[2]]$description == clowder_dataset]
  #save all information to a dataframe
  return(httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets/",df_id,"/files",apiKey)) %>% 
           httr::content() %>% {
             data.frame(
               size = purrr::map_chr(., "size"),
               date_created = purrr::map_chr(., "date-created"),
               filetype = purrr::map_chr(., "contentType"),       
               id = purrr::map_chr(., "id"),
               filename = purrr::map_chr(., "filename"),
               stringsAsFactors = F
             )
           }
  )
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
clowder_download_docs <- function(docData=NULL, outputDir, apiKey, limit=NULL){
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
        downloader::download(paste0("https://clowder.edap-cluster.com/api/files/", fileID[x], apiKey), 
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

#'@description A function to push a dataframe to a specified table in a database
#'@param df A dataframe to write to the database
#'@param tblName The name of the table to write the df data to
#'@import DBI
#'@return None
db_push_to_CvT <- function(df=NULL, tblName=NULL){
  if(is.null(df)) stop("Must provide a dataframe to push to database")
  if(is.null(tblName)) stop("Must provide database table name to write to")
  #Remove ID column because it'll be auto assigned in the push
  if(!nrow(df)){
    message("No data passed to push to CvT...returning")
    return()
  }
  df = df[!names(df) %in% c("id")]
  tryCatch({
    con = db_connect_to_CvT()
    dbWriteTable(con, value = df, name=c("cvt", "temp_tbl"), overwrite=TRUE, row.names=FALSE)  
    
    dbSendStatement(con, paste0("INSERT INTO cvt.", tblName, " (\"", paste0(names(df), collapse='","'),
                                "\") SELECT \"", paste0(names(df), collapse='","'), "\" FROM cvt.temp_tbl")) %T>% 
      dbClearResult()
    dbSendStatement(con, "DROP TABLE cvt.temp_tbl") %T>% 
      dbClearResult() #Drop temporary table
  },
  error=function(cond){ message("Error message: ", cond); return(NA) },
  warning=function(cond){ message("Warning message: ", cond); return(NULL) },
  finally={ dbDisconnect(con) }
  )
}

#'@description A function to pull table fk identification from a specified table
#'by a SQL filter statement, or the entire table.
#'@param tblName The name of the table to pull the ID from
#'@param idFilter A SQL WHERE statement to filter idName column to. If empty, pulls all data.
#'@import DBI
#'@return A list of ID values from the specified database table
db_get_tbl_id <- function(tblName=NULL, idFilter=NULL){
  if(is.null(tblName)) stop("Must provide database table name to write to")
  if(is.null(idFilter)) stop("Must provide an idFilter value to filter ID table by")
  #Remove ID column because it'll be auto assigned in the push
  tryCatch({
    con = db_connect_to_CvT()
    dbSendQuery(con, paste0("SELECT * FROM cvt.", tblName, " ", idFilter)) %T>%
      { dbFetch(.) ->> tmp } %>%
      dbClearResult()
    return(tmp)
  },
  error=function(cond){ message("Error message: ", cond); return(NA) },
  warning=function(cond){ message("Warning message: ", cond); return(NULL) },
  finally={ dbDisconnect(con) }
  )
}

#'@description A quick function to pull all database data into a dataframe list by table name.
db_check_CvTdb <- function(){
  con = db_connect_to_CvT()
  t_list = dbListTables(con)
  df = lapply(t_list, function(x){
    tbl(con, x) %>% collect()
  }) %T>% { names(.) <- t_list }
  dbDisconnect(con)
  return(df)
}
