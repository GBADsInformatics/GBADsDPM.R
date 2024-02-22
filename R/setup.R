setup <- function() {
  setwd(selectDirectory()) 
  
  file_path <- selectFile(caption = "Select a YAML file", filter = "*.yaml") 
  file_type <- "yaml"
  read_params(file_path = file_path, file_type = file_type)
  
}

