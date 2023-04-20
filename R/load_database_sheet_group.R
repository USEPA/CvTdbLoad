#' @description A function to load and pull all sheets from files already loaded to CvTdb.
#' It corrects for missing required column names from a template file by filling with NA values.
#' @param docID The CvT document ID for the file of interest
#' @param template_path The file path for the extraction template. If not supplied, hard coded columns will be used.
#' @import readxl magrittr
#' @return A dataframe of the combined sheets
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx], [excel_sheets][readxl::excel_sheets], [read_excel][readxl::read_excel]
#'  [rename][dplyr::rename], [left_join][dplyr::left_join], [select][dplyr::select], [mutate][dplyr::mutate], [rename_all][dplyr::rename_all], [filter][dplyr::filter], [distinct][dplyr::distinct]
#'  [all_of][tidyr::all_of]
#' @rdname load_database_sheet_group
#' @export 
#' @importFrom readxl read_xlsx excel_sheets read_excel
#' @importFrom dplyr rename left_join select mutate rename_all filter distinct
#' @importFrom tidyr all_of
load_database_sheet_group <- function(docID, template_path){
  sheet_list = c("documents", "studies", "subjects", "series", "conc_time_values")
  template_map = readxl::read_xlsx("input/template_map.xlsx")
  
  template = tryCatch({
    template_sheets = readxl::excel_sheets(template_path)
    lapply(template_sheets, function(s){
      tmp = paste0(s, ".", names(readxl::read_excel(template_path, sheet=s))) %>% 
        tolower()
      if(s == "Documents"){
        return(c(tmp, "documents.id"))
      } else if(s == "Studies"){
        return(c(tmp, "studies.fk_extraction_document_id"))
      } else if(s == "Series") {
        return(c(tmp, "time_units_original", "conc_units_original"))
      } else if(s == "Conc_Time_Values"){
        return(c(tmp, "time_original", "conc_original"))
      } else {
        return(tmp)
      }
    }) %T>% { names(.) <- template_sheets }   
  },
  error=function(cond){ message("...Error: ", cond); return(NULL) }
  )
  
  if(is.null(template)){ return(NA) } 
  #pull database data with template fields
  out = lapply(sheet_list, function(s){
    if(s == "documents"){
      tmp = db_query_cvt("SELECT * FROM documents WHERE extracted = 1") 
    } else {
      tmp = db_query_cvt(paste0("SELECT * FROM ", s))   
    }
    
    if(s == "conc_time_values"){
      tmp = dplyr::rename(tmp, conc_normalized = conc,
                          conc_lower_bound_normalized=conc_lower_bound,
                          conc_upper_bound_normalized=conc_upper_bound,
                          conc_sd_normalized = conc_sd)
    }
    tmp = tmp %T>% {
      #Have to map CvT database names back to the template (usually a _original stem)
      message("...Renaming mapped variables...", Sys.time())
      names(.)[names(.) %in% template_map$from[template_map$sheet == s]] <- dplyr::left_join(data.frame(from=names(.)[names(.) %in% 
                                                                                                                 template_map$from[template_map$sheet == s]], 
                                                                                                 stringsAsFactors = F), 
                                                                                      template_map[template_map$sheet==s,], 
                                                                                      by = "from") %>% 
        dplyr::select(to) %>% dplyr::mutate(to = as.character(to)) %>% unlist()
      message("...Returning raw data...", Sys.time()) 
    } %>% dplyr::rename_all(function(x){paste0(s,".", x)})
    
    #Get list of columns corresponding to a sheet from the template
    colList = switch(s, 
                     "documents" = template$Documents,
                     "studies" = template$Studies,
                     "subjects" = template$Subjects,
                     "series" = template$Series,
                     "conc_time_values" = template$Conc_Time_Values)
    #Fill missing columns with NA
    tmp[colList[!colList %in% names(tmp)]] <- NA
    message("Returning data for: ", s)
    return(dplyr::select(tmp, tidyr::all_of(colList)))
  }) %T>% { names(.) <- sheet_list }
  #Combine all data
  out = out$documents %>%
    dplyr::left_join(out$studies, by=c("documents.id"="studies.fk_extraction_document_id"), keep=TRUE) %>%
    dplyr::left_join(out$series, by=c("studies.id"="series.fk_study_id"), keep=TRUE) %>%
    dplyr::left_join(out$subjects, by=c("series.fk_subject_id"="subjects.id"), keep=TRUE) %>%
    dplyr::left_join(out$conc_time_values, by=c("series.id"="conc_time_values.fk_series_id"), keep=TRUE)
  #Split into subgroups
  out = lapply(unique(out$documents.id), function(d){
    doc = out %>% dplyr::filter(out$documents.id == d)
    lapply(template_sheets, function(s){
      #Get list of columns corresponding to a sheet from the template
      colList = switch(s, 
                       "Documents" = template$Documents,
                       "Studies" = template$Studies,
                       "Subjects" = template$Subjects,
                       "Series" = template$Series,
                       "Conc_Time_Values" = template$Conc_Time_Values)
      #Fill missing columns with NA
      doc[colList[!colList %in% names(doc)]] <- NA
      doc =  dplyr::select(doc, tidyr::all_of(colList)) %>% 
        dplyr::distinct() %T>%{ #Important T-operator
          colnames(.) = sub('.*\\.', '', colnames(.)) #Remove Prefixes
        }
    }) %T>% { names(.) <- template_sheets }
  }) %T>% { names(.) <- unique(out$documents.id ) }
    
  
  for(i in seq_len(length(names(out)))){
    # if(i < 24){#Quick skip for testing purposes
    #   next
    # }
    #
    
    f = paste0("load_CvT_doc_id_", names(out)[i])
    # if(!f %in% paste0("load_CvT_doc_id_", c(25, 56, 192, 138, 141))){
    #   next
    # }
    ######insert loop over fileList logic########
    message("Pushing file (", i, "/", length(names(out)),"): ", f, "...", Sys.time())
    #Create/clear log entry for filename
    log_CvT_doc_load(f, m=NULL, reset=TRUE)
    #Load Documents Sheet
    doc_sheet_list = out[[i]]
    
    doc_sheet_list$Subjects$species = normalize_species(x=doc_sheet_list$Subjects$species)
    
    #Call to the orchestration function for data normalization (with error logging)
    doc_sheet_list = normalize_CvT_data(df=doc_sheet_list, f=f)
    
    #If any issues were logged during normalization, don't push the doc
    if(log_check(basename(f))){
      message("...file has logged issues...skipping doc")
      next
    }
  }
}
