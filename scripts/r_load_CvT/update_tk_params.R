library(readr); library(dplyr)
cvt_in = readr::read_delim("input\\tk params\\SupTable-CvTData.txt")
tk_params = readr::read_delim("input\\tk params\\SupTable-TKFits.txt") %>% 
  select(DTXSID, Compound, CAS, Species, Reference, Model, halflife) %>%
  distinct()
