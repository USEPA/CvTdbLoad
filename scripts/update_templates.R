#Script of functions to add/remove columns to/from CvT templates
library(readxl); library(writexl); library(dplyr); library(magrittr)

load_file <- function(f_path){
  f_sheets = readxl::excel_sheets(f_path)
  
  f = lapply(f_sheets, function(x){
    #https://readxl.tidyverse.org/articles/articles/column-names.html
    readxl::read_excel(f_path, sheet=x, 
                       .name_repair = ~ ifelse(nzchar(.x), .x, paste0("missing_col_", LETTERS[seq_along(.x)])))
  }) %T>% { names(.) <- f_sheets }
  
  return(f)
}
#https://stackoverflow.com/questions/13502601/add-insert-a-column-between-two-columns-in-a-data-frame
#https://stackoverflow.com/questions/51162023/dplyr-mutate-new-column-at-a-specified-location
#https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
addCol <- function(df, newCol, value, neighbor, after=TRUE){
  if(newCol %in% names(df)){
    #message("...'", newCol, "' already exists...returning...")
    return(df)
  }  
  message("...adding '", newCol, "' to the ", ifelse(after, "right", "left"), 
          " of '", neighbor, "'")
  if(after){
      return(mutate(df, !!newCol := value, .after=!!neighbor))
  } else {
    return(mutate(df, !!newCol := value, .before=!!neighbor))
  }
}

delCol <- function(df, delCol){
  message("...deleting '", paste0(delCol, collapse=", "), "' column...")
  return(df[, !names(df) %in% delCol])
}

#"L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\Needs Admin Check"
#"L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\Needs Further Curation"
#"L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\Reviewer Disagreement"
#"L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\1_qa_format_complete"
#"L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\toQA"

f_list = list.files("L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA",
                    pattern=".xlsx",#"_CvT_", 
                    full.names = TRUE,
                    recursive = TRUE)
f_list = f_list[!grepl("qa_log", f_list)]
#f_list = list.files("C:\\Users\\JWALL01\\Desktop\\CvT Japan", full.names = TRUE)
#Load template
template_path = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\CvT_data_template_articles.xlsx"
template = tryCatch({
  template_sheets = readxl::excel_sheets(template_path)
  lapply(template_sheets, function(s){
    names(readxl::read_excel(template_path, sheet=s))
  }) %T>% { names(.) <- template_sheets }   
},
error=function(cond){ message("...Error: ", cond); return(NULL) }
)

bad_doc = list()
for(f_path in f_list){
  message(" --- Checking File: ", f_path, "---")
  #f_path = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\1_qa_format_complete\\PMID1855490_CvT_data_template_articles_MH.xlsx"
  f = tryCatch({
    load_file(f_path)},
    error=function(cond){
      bad_doc[f_path] = cond
      message("Error: ", cond)
      return(NULL)
    })
  
  if(!is.null(f)){
    #Check if any changes are necessary
    check = lapply(names(f), function(s){
      extra = names(f[[s]])[!names(f[[s]]) %in% template[[s]]]
      missing = template[[s]][!template[[s]] %in% names(f[[s]])]
      return(!length(extra) & !length(missing))
    }) %>% unlist()
    
    if(all(check)){
      message("...no changes necessary...skipping...")
      next
    }
    #Extra sheet
    if(any(!names(f) %in% names(template))){
      if(!"CompTox" %in% names(f)){#Allow Japan template through
        bad_doc[f_path] = paste0("Extra Sheet: ", paste0(names(f)[!names(f) %in% names(template)], collapse=", "))
        next
      }
    }
    #Missing sheet
    if(any(!names(template) %in% names(f))){
      bad_doc[f_path] = paste0("Missing Sheet: ", paste0(names(template)[!names(template) %in% names(f)], collapse=", "))
      next
    }
    
    for(s in names(f)){
      if(any(grepl("missing_col_", names(f[[s]])))){
        m = paste0("...sheet (", s,") ", f_path, " has columns without names...must manually fix first...")
        message(m)
        bad_doc[f_path] = m
        break #Stop checking
      }
    }
    #Check for multiple documents but missing id and fk_reference_document_id
    if(all(c("Documents", "Studies") %in% names(f))){
      m = "...manually curate multiple documents..."
      if(nrow(f$Documents) > 1){
        if(!"id" %in% names(f$Documents) | !"fk_reference_document_id" %in% names(f$Studies)){
          bad_doc[f_path] = m
        } else {
          if(any(is.na(f$Documents$id))){
            bad_doc[f_path] = m
          }
        }
      }
    }
    if(f_path %in% names(bad_doc)){
      next #Skip if added to bad_doc in the previous loop
    }
  } else { #Error loading file
    next
  }
  
  
  for(s in names(f)){
    #Select dictionary of columns to add for a given sheet
    c_list = switch(
      s,
      "Documents" = list(document_type=list(neighbor="pmid", after=FALSE),
                         id=list(neighbor="document_type", after=FALSE),
                         other_study_identifier=list(neighbor="pmid", after=TRUE),
                         url=list(neighbor="title", after=TRUE),
                         curator_comment=list(neighbor="url", after=TRUE)
      ),
      "Studies" = list(fk_reference_document_id=list(neighbor="test_substance_name", after=FALSE),
                       author_comment=list(neighbor="curator_comment", after=FALSE),
                       aerosol_particle_density_units=list(neighbor="curator_comment", after=TRUE),
                       aerosol_particle_density=list(neighbor="curator_comment", after=TRUE),
                       aerosol_particle_diameter_units=list(neighbor="curator_comment", after=TRUE),
                       aerosol_particle_diameter_gsd=list(neighbor="curator_comment", after=TRUE),
                       aerosol_particle_diameter_mean=list(neighbor="curator_comment", after=TRUE),
                       dermal_applied_area_units=list(neighbor="curator_comment", after=TRUE),
                       dermal_applied_area=list(neighbor="curator_comment", after=TRUE),
                       dermal_dose_vehicle_pH=list(neighbor="curator_comment", after=TRUE),
                       #dermal_dose_vehicle=list(neighbor="curator_comment", after=TRUE),
                       test_substance_name_secondary=list(neighbor="test_substance_name", after=TRUE),
                       test_substance_casrn=list(neighbor="test_substance_name_secondary", after=TRUE),
                       dose_duration_units=list(neighbor="dose_duration", after=TRUE)
                       ),
      "Series" = list(figure_type=list(neighbor="figure_name", after=TRUE),
                      log_conc_units=list(neighbor="conc_units", after=TRUE),
                      lod=list(neighbor="loq_units", after=TRUE),
                      lod_units=list(neighbor="lod", after=TRUE),
                      y_max=list(neighbor="figure_series_identifier", after=TRUE),
                      y_min=list(neighbor="figure_series_identifier", after=TRUE),
                      x_max=list(neighbor="figure_series_identifier", after=TRUE),
                      x_min=list(neighbor="figure_series_identifier", after=TRUE),
                      analyte_name_secondary=list(neighbor="analyte_name", after=TRUE),
                      analyte_casrn=list(neighbor="analyte_name_secondary", after=TRUE)
        
      ),
      "Subjects" = list(age_units=list(neighbor="age", after=TRUE),
                        height_units=list(neighbor="height", after=TRUE),
                        weight_units=list(neighbor="weight", after=TRUE)
        
      ),
      "Conc_Time_Values" = list(conc_sd=list(neighbor="conc", after=TRUE),
                                conc_lower_bound=list(neighbor="conc_sd", after=TRUE),
                                conc_upper_bound=list(neighbor="conc_lower_bound", after=TRUE),
                                curator_comment=list(neighbor="conc_upper_bound", after=TRUE)
      )
    )
    #Select dictionary of columns to delete for a given sheet
    d_list = switch(
      s,
      "Studies" = c("fk_extraction_document_id", "author_comments", "dermal_dose_vehicle"), #misspelled field
      "Series" = c("log_concentration_units") #misspelled field
    )
    
    #Delete all columns in d_list (if it isn't empty)
    if(!is.null(d_list)){
      f[[s]] = f[[s]] %>% delCol(delCol=d_list)
    }
    #Add all columns in c_list (if it isn't empty)
    if(!is.null(c_list)){
      #Loop through all columns to add
      for(c in names(c_list)){
        f[[s]] = f[[s]] %>%
          addCol(newCol=c, value=NA, 
                 neighbor = c_list[[c]]$neighbor, 
                 after = c_list[[c]]$after)
      }
    }
  }
  
  check = lapply(names(f), function(s){
    if(s == "CompTox"){#Allow Japan skip
      return(FALSE)
    }
    extra = names(f[[s]])[!names(f[[s]]) %in% template[[s]]]
    missing = template[[s]][!template[[s]] %in% names(f[[s]])]
    message("...", s, " Extra: ", paste(extra, collapse="; "))
    message("...", s, " Missing: ", paste(missing, collapse="; "))
    return(length(extra) | length(missing))
  }) %>% unlist()
  
  if(any(check)){
    bad_doc[f_path] = "Extra or Missing columns after update attempt"
    next
  }
  #Add document ID and type
  f$Documents$id = 1:nrow(f$Documents)
  if(nrow(f$Documents) == 1){#Only for single docs
    f$Documents$document_type = 1  
  }
  #Write changes back to file
  #writexl::write_xlsx(x=f, path=f_path)
}
