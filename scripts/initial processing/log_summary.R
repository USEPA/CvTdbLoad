#Script to summarize logged document issues
log = readxl::read_xlsx("output\\CvT_loading_log.xlsx") %>%
  select(-timestamp)

summ_log = log %>%
  tidyr::pivot_longer(cols=!filename, names_to="flags") %>%
  filter(value != 0) %>%
  group_by(flags) %>%
  summarise(n=n())

summ_log_file = log %>%
  tidyr::pivot_longer(cols=!filename, names_to="flags") %>%
  filter(value != 0) %>%
  select(flags, filename)
