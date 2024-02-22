#' @title
#' Setup and run DPM/AHLE scenarios

#' @description
#' Read in a DPM/AHLE parameter file in YAML format and run compartmental model. 
#' Users are prompted to select a directory in which to save results, and a file to be read.
#' 
#' @param seed_value An integer value of any length for reproducibility
#' 
#' @example
#' # setup(seed_value = NULL)

setup <- function(seed_value = NULL) {
  setwd(selectDirectory()) 
  
  file_path <- selectFile(caption = "Select a YAML file", filter = "*.yaml") 
  file_type <- "yaml"
  read_params(file_path = file_path, file_type = file_type)
  run_compartmental_model(seed_value = seed_value)
}

