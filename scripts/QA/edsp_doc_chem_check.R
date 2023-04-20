#ERMODEL chemical list
out = db_query_cvt("select pmid, other_study_identifier, tentatively_identified_test_substance from cvt.documents where extracted != 1 and tentatively_identified_test_substance is not NULL")

bracket_out = out %>%
  filter(grepl("\\[", tentatively_identified_test_substance))

out = out %>%
  filter(!pmid %in% bracket_out$pmid)

semicolon_out = out %>%
  filter(grepl(";", tentatively_identified_test_substance))

out = out %>%
  filter(!pmid %in% semicolon_out$pmid)

# Process semicolon splits
tmp = semicolon_out$tentatively_identified_test_substance %>%
  stringr::str_split_fixed(., ";", n=Inf) %>%
  as.data.frame()

semicolon_out = semicolon_out %>%
  select(-tentatively_identified_test_substance) %>%
  cbind(., tmp) %>%
  pivot_longer(cols=-c(pmid, other_study_identifier), values_to = "chemical_name") %>%
  select(-name) %>%
  mutate(chemical_name = stringr::str_squish(chemical_name)) %>%
  distinct()

# Process bracket splits
bracket_out$tentatively_identified_test_substance = bracket_out$tentatively_identified_test_substance %>%
  gsub("\\[ '", "", .) %>%
  gsub("\\[' ", "", .) %>%
  gsub("\\['", "", .) %>%
  gsub("'\\]", "", .) %>%
  gsub("' \\]", "", .) %>%
  gsub("\\[ \"", "", .) %>%
  gsub("\\[\" ", "", .) %>%
  gsub("\\[\"", "", .) %>%
  gsub("\"\\]", "", .) %>%
  gsub(" ', \\]", "", .) %>%
  stringr::str_squish()

tmp = bracket_out$tentatively_identified_test_substance %>%
  stringr::str_split_fixed(., "', '", n=Inf) %>%
  as.data.frame()

bracket_out = bracket_out %>%
  select(-tentatively_identified_test_substance) %>%
  cbind(., tmp) %>%
  pivot_longer(cols=-c(pmid, other_study_identifier), values_to = "chemical_name") %>%
  select(-name) %>%
  mutate(chemical_name = stringr::str_squish(chemical_name)) %>%
  distinct()

out = rbind(out %>%
              dplyr::rename(chemical_name = tentatively_identified_test_substance), 
            semicolon_out, 
            bracket_out) %>%
  filter(chemical_name != "") %>%
  filter(!is.na(pmid) | !is.na(other_study_identifier))

writexl::write_xlsx(out, "edsp_cvt_doc_chems_check_20230118.xlsx")

# Load EDSP list
"C:\Users\jwall01\OneDrive - Environmental Protection Agency (EPA)\Profile\Downloads\Chemical List ERMODEL-2023-01-18.xlsx"
"C:/Users/jwall01/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/cvtdb-load"

# Load original list (https://comptox.epa.gov/dashboard/chemical-lists/ERMODEL)
edsp = readxl::read_xlsx("../../Downloads/Chemical List ERMODEL-2023-01-18.xlsx")
# Load batch search matched CvT
cvt_edsp = readxl::read_xlsx("../../Downloads/EDSP_CCD-Batch-Search_2023-01-18_06_58_47.xlsx", sheet = "Main Data") %>%
  filter(!is.na(DTXSID)) %>%
  select(INPUT, DTXSID)

# Check for overlap
mapped_out = out %>%
  left_join(cvt_edsp, 
            by=c("chemical_name"="INPUT")) %>%
  filter(!is.na(DTXSID)) %>%
  filter(DTXSID %in% unique(edsp$DTXSID))

message("Unique chemical overlap count: ", length(unique(mapped_out$chemical_name)))
message("Potential document count: ", length(unique(mapped_out$pmid)))