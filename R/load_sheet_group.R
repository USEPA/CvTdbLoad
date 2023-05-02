#' @description A function to load and pull all sheets from files in the specified directory.
#' It corrects for missing required column names from a template file by filling with NA values.
#' @param fileName The file name or path for the file of interest
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
#'  [excel_sheets][readxl::excel_sheets], [read_excel][readxl::read_excel], [read_xlsx][readxl::read_xlsx]
#'  [select][dplyr::select]
#'  [all_of][tidyr::all_of]
#' @rdname load_sheet_group
#' @export 
#' @importFrom readxl excel_sheets read_excel read_xlsx
#' @importFrom dplyr select
#' @importFrom tidyr all_of
load_sheet_group <- function(fileName="", template_path=""){
  
  template = tryCatch({
    template_sheets = readxl::excel_sheets(template_path)
    lapply(template_sheets, function(s){
      names(readxl::read_excel(template_path, sheet=s))
    }) %T>% { names(.) <- template_sheets }   
  },
  error=function(cond){ message("...Error: ", cond); return(NULL) }
  )
  
  if(is.null(template)){
    message("...passed template file path doesn't exist...using default template...")
    template = list("Documents" = c("pmid", "other_study_identifier", "doi", "first_author", 
                                    "year", "title","url", "curator_comment"),
                    "Studies" = c("id", "test_substance_name", "test_substance_name_secondary", 
                                  "test_substance_casrn", "dose_level", "dose_level_units", 
                                  "administration_route", "dose_duration", "dose_frequency",     
                                  "dose_vehicle", "dose_volume", "fasting_period", "author_comment", 
                                  "curator_comment", "dermal_dose_vehicle", "dermal_dose_vehicle_pH", 
                                  "dermal_applied_area", "dermal_applied_area_units",
                                  "aerosol_particle_diameter_mean", "aerosol_particle_diameter_gsd", 
                                  "aerosol_particle_diameter_units", "aerosol_particle_density",
                                  "aerosol_particle_density_units"),
                    "Subjects" = c("id", "species", "subtype", "sex", "age", "age_units", "age_category", 
                                   "height", "height_units", "weight", "weight_units", "curator_comment"),
                    "Series" = c("id", "analyte_name", "analyte_name_secondary", 
                                 "analyte_casrn", "figure_name", "figure_type", 
                                 "figure_series_identifier", "x_min", "x_max", "y_min", "y_max", 
                                 "time_units", "conc_units", "log_conc_units", "loq", "loq_units", 
                                 "lod", "lod_units", "analytical_method_detail", 
                                 "radiolabeled", "fk_study_id", "fk_subject_id", "n_subjects_in_series", 
                                 "conc_medium", "curator_comment"),
                    "Conc_Time_Values" = c("fk_series_id", "time", "conc", "conc_sd", "conc_lower_bound", 
                                           "conc_upper_bound", "curator_comment"))
  }
  
  tryCatch({
    sheetNames = readxl::excel_sheets(fileName)
    sheetNames = sheetNames[sheetNames %in% names(template)]
    lapply(sheetNames, function(x){
      if(!x %in% names(template)){
        return(NULL)
      }
      tmp = readxl::read_xlsx(fileName, sheet = x, col_types = "text",
                              .name_repair = ~ ifelse(nzchar(.x), .x, paste0("missing_col_", LETTERS[seq_along(.x)])))
      #Get list of columns corresponding to a sheet from the template
      colList = switch(x, 
                       "Documents" = template$Documents,
                       "Studies" = template$Studies,
                       "Subjects" = template$Subjects,
                       "Series" = template$Series,
                       "Conc_Time_Values" = template$Conc_Time_Values)
      #Fill missing columns with NA
      tmp[colList[!colList %in% names(tmp)]] <- NA
      tmp =  dplyr::select(tmp, tidyr::all_of(colList))
    }) %T>% {
      names(.) <- sheetNames
    }
    
  },
  error=function(cond){ message("Error message: ", cond); return(NULL) }
  ) %>% return()
}
