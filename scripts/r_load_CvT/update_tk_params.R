library(readr); library(dplyr)
cvt_in = readr::read_delim("input\\tk params\\SupTable-CvTData.txt", show_col_types = FALSE)
tk_params = readr::read_delim("input\\tk params\\SupTable-TKFits.txt", show_col_types = FALSE) %>% 
  select(DTXSID, Compound, CAS, Species, Reference, Model, halflife) %>%
  distinct()
