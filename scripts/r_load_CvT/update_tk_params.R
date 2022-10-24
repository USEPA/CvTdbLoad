library(readr); library(dplyr)

#' update_tk_params
#' @param tk_input String filepath to the input file used for tk params
#' @param tk_output String filepath to the output file of tk params
#' @return None. Data is processed and database updates are made.
update_tk_params(tk_input="input\\tk params\\SupTable-CvTData.txt", 
                 tk_output="input\\tk params\\SupTable-TKFits.txt"){
  # Load the input data
  tk_input = readr::read_delim(tk_input, show_col_types = FALSE)
  # Load the output data
  tk_output = readr::read_delim(tk_output, show_col_types = FALSE) %>% 
    select(DTXSID, Compound, CAS, Species, Reference, Model, halflife) %>%
    distinct()
  
  message("Code using bad assumption for now to join input and output by chemical information")
  warning("Code using bad assumption for now to join input and output by chemical information")
  
  out = tk_input %>%
    left_join(tk_output, 
              by=c("DTXSID", "CAS", "Compound", "Species")
              )
}


