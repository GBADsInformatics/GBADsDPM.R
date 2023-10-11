#' @title
#' File format conversion  
#' 
#' @description
#' Converts a YAML file to a JSON file
#' 
#' @example 
#' # convert_yaml_to_json()
#' 

convert_yaml_to_json <- function(file_path) {
  yaml_data <- read_yaml(file.choose())
  json_data <- toJSON(yaml_data, auto_unbox = TRUE, pretty = TRUE)
  write_json(json_data, path = file_path)
}