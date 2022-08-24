
normalize_species <- function(x){
  message("...normalizing species...")
  #Convert species
  x = tolower(x)
  conv = list(dog=list("dog", "dogs"),
              human=list("human", "humans", "female", "male", "females", "males", "woman", "women", "man", "men"),
              mouse=list("mouse", "mice", "mouses"),
              `monkey`=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys",
                                      "rhesus monkeys (macaca mulatta)", "rhesus monkeys",
                                      "cynomolgus monkeys (macaca fascicularis)"),
              rat=list("rat", "rats"),
              rabbit = list("rabbit", "rabbits"),
              `guinea pig` = list("guinea pig", "guinea pigs"),
              frog = list("frog", "frogs")
  )
  
  x = lapply(x, function(s){
    for(c in names(conv)){
      if(!s %in% conv[[c]]){
        next
      } else {
        return(c)  
      }
    }
    return(s)
  }) %>% unlist()
  return(x)
}

get_unique_species_to_curate <- function(fileList, template_path){
  #Get species from files
  spec = lapply(fileList, function(f){
    s_list = load_sheet_group(fileName = f, template_path = template_path)
    s_list$Subjects %>% select(species) %>% unique() %>% unlist() %>% unname()
  }) %>% 
    unlist()
  
  out = normalize_species(spec) %>%
    unique()
  return(out[!out %in% c("dog", "human", "mouse", "nonhuman primate", 
                         "rat", "rabbit", "guinea pig", "frog")])
}