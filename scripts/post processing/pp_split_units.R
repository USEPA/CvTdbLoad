#' @title pp_split_units
#' @description Postprocessing function to flag units to split from numeric fields as needed.
#' @param schema_name PostgreSQL schema to query
#' @return Output log of candidates to split units.
#' @seealso 
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_count}}, \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_remove}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname pp_split_units
#' @export 
#' @importFrom dplyr filter select rename mutate bind_rows
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish str_count str_detect str_remove_all str_extract_all str_extract
#' @importFrom tidyr separate unite
#' @importFrom writexl write_xlsx
pp_split_units <- function(schema_name){
  # Pull tables of interest  
  tbl_list = db_query_cvt(paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = '",
                                 schema_name,"' AND table_type = 'BASE TABLE'")) %>%
    # Filter out dictionary, audit tables, etc.
    dplyr::filter(!grepl("dict|chemical|audit|tk_param", table_name)) %>%
    .[[1]]
  
  age_category_regex = readxl::read_xlsx("input/dictionaries/age_category_dict.xlsx") %>%
    dplyr::select(-species, -unit) %>% 
    names() %>%
    paste0(collapse="|")
  
  # Empty lists to store cases
  unit_data = list()
  unhandled = data.frame()
  # Loop through each table of interest
  for(tbl_n in tbl_list){
    message("Processing table: ", tbl_n)
    # Get potential columns of interest based on "units" fields
    u_fields = db_query_cvt(paste0("SELECT * FROM ", schema_name, ".", tbl_n, " LIMIT 1")) %>%
      names() %T>% {
        # Cache full table names to filter against
        tbl_n_names <<- .
      } %>%
      # Filter to units columns
      .[grepl("units", .)] %>%
      # Create dataframe of matching fields
      data.frame(value=gsub("_units", "", .), units=.) %>%
      # Filter to only fields within the database table
      dplyr::filter(value %in% tbl_n_names, units %in% tbl_n_names)
    
    # Remove intermediate not needed
    # rm(tbl_n_names)
    
    if(!nrow(u_fields)) {
      message("...No 'units' fields found in ", tbl_n, "...skipping...")
      next
    }
    
    # Loop through each unit-value pair and normalize
    for(r in seq_len(nrow(u_fields))){
      # Select fields of interest 
      tmp = db_query_cvt(paste0("SELECT id, ", 
                                u_fields[r,] %>% unlist() %>% sort() %>% toString(), 
                                ", curator_comment FROM ", schema_name, ".", tbl_n)) %>%
        # Filter to those that contain unit strings in their value field
        dplyr::filter(grepl("[A-Za-z]", !!as.symbol(u_fields$value[r])),
               # Value field must contain a numeric value
               grepl("[0-9]", !!as.symbol(u_fields$value[r]))) %>%
        dplyr::rename(id=id) %>%
        # Add logic to count whitespace, filter out those with > 1
        dplyr::mutate(split_value := !!as.symbol(u_fields$value[r]) %>%
                 stringr::str_squish(),
               ws_count = stringr::str_count(split_value, "[:space:]"),
               curator_comment_new = NA)
      
      # Perform simple substitutions
      # Replace "at least " with ">"
      tmp = tmp %>% dplyr::mutate(split_value = gsub("at least ", ">=", split_value))
      # Replace "at most " with "<"
      tmp = tmp %>% dplyr::mutate(split_value = gsub("at most ", "<=", split_value))
      # Replace .D with .
      tmp = tmp %>% dplyr::mutate(split_value = gsub("([0-9]+)\\.D([0-9]+)", "\\1.\\2", split_value))
      # Remove " - seems too low" and add it to a curator comment, if it exists
      tmp = tmp %>% 
        dplyr::mutate(curator_comment_new = ifelse(stringr::str_detect(split_value, " - seems too low"),
                                        paste0(u_fields$value[r], " seems too low"), 
                                        curator_comment_new),
               split_value = stringr::str_remove_all(split_value, " - seems too low"))
      
      # Remove " free base" and add it to a curator comment, if it exists
      tmp = tmp %>% 
        dplyr::mutate(curator_comment_new = ifelse(stringr::str_detect(split_value, " free base"),
                                        "free base", curator_comment_new),
               split_value = stringr::str_remove_all(split_value, " free base"))
      
      # Remove age_category and add it to a curator comment, if it exists
      tmp = tmp %>% 
        dplyr::mutate(curator_comment_new = ifelse(stringr::str_detect(split_value, 
                                                            age_category_regex),
                                        stringr::str_extract_all(split_value, age_category_regex), 
                                        curator_comment_new),
               split_value = stringr::str_remove_all(split_value, age_category_regex) %>%
                 stringr::str_squish())
      # Rectify split decimals (24. 04 to 24.04)
      tmp = tmp %>% dplyr::mutate(split_value = gsub("(\\d+) \\.(\\d+)", "\\1.\\2", split_value))
      
      # Calculate whitespace count
      tmp = tmp %>% dplyr::mutate(ws_count = stringr::str_count(split_value, "[:space:]"))
      
      out = list()
      
      # Filter to simple split cases (start with numeric and only one whitespace)
      out$simple_split = tmp %>%
        dplyr::filter(grepl("^[0-9]|^[\\.]|^[>]", split_value), 
               ws_count == 1) %>%
        tidyr::separate(split_value, into=c("split_value", "split_units"), sep=" ")
      tmp = tmp %>% dplyr::filter(!id %in% out$simple_split$id)
      
      # Handle years old units
      out$years_old = tmp %>%
        dplyr::filter(grepl("years old|yo", split_value)) %>%
        dplyr::mutate(split_units = "years",
               split_value = gsub("years old|yo", "", split_value) %>%
                 stringr::str_squish())
      tmp = tmp %>% dplyr::filter(!id %in% out$years_old$id)
      
      # Handle weeks old units
      out$weeks_old = tmp %>%
        dplyr::filter(grepl("weeks old", split_value)) %>%
        tidyr::separate(col=split_value, into=c("split_value", "split_units"), sep=" ", extra="merge")
      tmp = tmp %>% dplyr::filter(!id %in% out$weeks_old$id)
      
      # Handle or, +/-, etc.
      out$range = tmp %>%
        # Ensure it's numbers separated by or, +/-, or -
        dplyr::filter(grepl("[0-9] or [0-9]|[0-9] ± [0-9]|[0-9] +-[0-9]|[0-9] - [0-9]", split_value))
      if(nrow(out$range)){
        out$range = out$range %>%
          dplyr::mutate(split_units = gsub("^.* ", "", split_value))
        rem_u = unique(out$range$split_units)
        # Regex to remove units from value field, order by character so "g" isn't removed before "kg"
        out$range$split_value = gsub(paste0(rem_u[order(nchar(rem_u), rem_u, decreasing = TRUE)], collapse="|"), 
                                     "", out$range$split_value) %>% 
          stringr::str_squish()
      }
      tmp = tmp %>% dplyr::filter(!id %in% out$range$id)
      
      # Case for 2.5-150.2g or equivalent with ranged whole or decimal numbers
      out$range_2 = tmp %>%
        # https://stackoverflow.com/questions/22658055/regular-expression-for-number-with-a-hyphen-and-a-decimal
        dplyr::filter(grepl("^[0-9]*.?[0-9]*-[0-9]*.?[0-9][A-za-z\\s]$", split_value)) %>%
        dplyr::mutate(split_units = sub("[0-9]*.?[0-9]*-[0-9]*.?[0-9]", "", split_value),
               split_value = sub("[^0-9.-]", "", split_value))
      tmp = tmp %>% dplyr::filter(!id %in% out$range_2$id)
      
      # Handle case like 27.82 (2.13) g
      out$parenthetic_1 = tmp %>%
        dplyr::filter(grepl(") [A-Za-z]+$", split_value)) %>%
        dplyr::mutate(split_units = sub('.*\\)', '', split_value) %>%
                 stringr::str_squish(),
               split_value = sub('\\).*', ')', split_value) %>%
                 stringr::str_squish())
      tmp = tmp %>% dplyr::filter(!id %in% out$parenthetic_1$id)
      
      # Handle case like 72.6-90.7 kg (mean 83.1)
      out$parenthetic_2 = tmp %>%
        dplyr::filter(grepl("[0-9] [A-Za-z]+\\s\\(mean", split_value)) %>%
        dplyr::mutate(split_units = stringr::str_extract(split_value, "\\s[A-Za-z]+\\s\\(") %>%
                 sub("\\($", "", .) %>%
                 stringr::str_squish(),
               split_value=sub("\\s[A-Za-z]+\\s\\(", " (", split_value) %>%
                 stringr::str_squish())
      tmp = tmp %>% dplyr::filter(!id %in% out$parenthetic_2$id)
      
      # Handle case like 
      out$parenthetic_3 = tmp %>%
        dplyr::filter(grepl("[0-9] [A-Za-z]+\\s\\([0-9]*.?[0-9]*-[0-9]*.?[0-9]\\)$", split_value)) %>%
        dplyr::mutate(split_units = stringr::str_extract(split_value, "\\s[A-Za-z]+\\s\\(") %>%
                 sub("\\($", "", .) %>%
                 stringr::str_squish(),
               split_value=sub("\\s[A-Za-z]+\\s\\(", " (", split_value) %>%
                 stringr::str_squish())
      tmp = tmp %>% dplyr::filter(!id %in% out$parenthetic_3$id)
      
      # Handle case like (23-29y)
      out$parenthetic_4 = tmp %>%
        dplyr::filter(grepl("\\([0-9]+\\.?[0-9]*-[0-9]+\\.?[0-9]*[a-zA-Z]+\\)$", split_value)) %>%
        dplyr::mutate(
          split_units = stringr::str_extract(split_value, "[a-zA-Z]+(?=\\)$)"),
          split_value = sub("\\(([0-9]+\\.?[0-9]*)-([0-9]+\\.?[0-9]*)[a-zA-Z]+\\)$", "\\1-\\2", split_value)
        )
      tmp = tmp %>% dplyr::filter(!id %in% out$parenthetic_4$id)
      
      # Handle case like "6 hr 10min", "2 hr - 20 min"
      out$partial_hours = tmp %>%
        dplyr::filter(grepl("^\\d+\\s*hr\\s*-?\\s*\\d+\\s*min$", split_value)) %>%
        dplyr::mutate(
          clean_value = gsub(" |-", "", split_value),
          hours = as.numeric(stringr::str_extract(clean_value, "^[0-9]+(?=hr)")),
          minutes = as.numeric(stringr::str_extract(clean_value, "(?<=hr)[0-9]+(?=min)")),
          split_value = as.character(ifelse(!is.na(minutes),
                                            sprintf("%f", hours + minutes / 60),
                                            sprintf("%d", hours))),
          split_units = "hr"
        )
      tmp = tmp %>% dplyr::filter(!id %in% out$partial_hours$id)
      
      # Recombine for return
      out = dplyr::bind_rows(out) %>%
        # Add table name
        dplyr::mutate(tbl_name = tbl_n,
               curator_comment_new = unlist(curator_comment_new),
               split_units = split_units %>%
                 # Remove ending punctuation to units
                 gsub("[[:punct:]]$", " ", .)) %>%
        #Combine curator_comment fields
        tidyr::unite(col="curator_comment", curator_comment, curator_comment_new, 
                     sep = ", ", na.rm = TRUE)
      # Append to output to review
      if(nrow(out)){
        unit_data[[paste0(tbl_n,"_",u_fields$value[r])]] = out  
      }
      
      # Track unhandled cases
      if(nrow(tmp)){
        message("...Unhandled cases: ", tmp$split_value %>% unique() %>% toString())  
        unhandled = tmp %>%
          # select(split_value) %>% 
          # unique() %>%
          dplyr::mutate(table_name = tbl_n, field_name = u_fields$value[r]) %>%
          dplyr::select(id, table_name, field_name, case=split_value) %>%
          rbind(unhandled, .)
      }
    }
  }
  
  unit_data$unhandled = unhandled
  message("Exporting results...")
  writexl::write_xlsx(unit_data, paste0("output/cvt_split_units_check_", Sys.Date(), ".xlsx"))
}
