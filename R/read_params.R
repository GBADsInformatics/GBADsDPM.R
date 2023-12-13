#' @title
#' Import AHLE parameter file 

#' @description
#' Read in AHLE parameter file in YAML format
#' 
#' @param file_path Relative or absolute path to file
#' @param file_type File type (Currently only YAML is supported)
#' 
#' @example
#' # Read_parameters_from_file(file_path = "path/to/params.yaml", file_type = "yaml")

read_params <- function(file_path, file_type = "yaml") {
  # Error handling for incorrect file type
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (file_type != "yaml") {
    stop("Invalid file type. Supported file type: 'yaml'")
  }
  
  # Read in YAML file
  params_data <- read_yaml(file_path)
  
  # Recursively evaluate R expressions within a list
  evaluate_r_expressions <- function(data) {
    if (is.list(data)) {
      for (key in names(data)) {
        data[[key]] <- evaluate_r_expressions(data[[key]])
      }
    } else if (is.character(data)) {
      if (grepl("^r\\w*\\(", data)) {
        data <- eval(parse(text = data))
      } else if (identical(gsub("\\s+", "", data), as.character(parse(text = data)))) {
        data <- eval(parse(text = data))
      } else if (grepl("^\\d+\\s*[-+*/]\\s*\\d+$", data)) {
        data <- eval(parse(text = data))
      }
    }
    return(data)
  }
  
  # Evaluate R expressions within the parameters data
  params_data <- evaluate_r_expressions(params_data)
  
  # Loop through the names and values and set the parameters accordingly
  for (parameter in names(params_data)) {
    value <- params_data[[parameter]]
    
    # Set the parameter value using assign()
    assign(parameter, value, envir = parent.frame())
  }
}