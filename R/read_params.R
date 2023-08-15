#' Import parameter file 
#' 
#' Read in parameter file in CSV, JSON, or YAML format.
#' 
#' @param file_path Relative or absolute path to file
#' @param file_type File type (One of: CSV, JSON, or YAML)
#' 
#' @example
#' # read_parameters_from_file(file_path = "path/to/params.csv", file_type = "csv")
#' # read_parameters_from_file(file_path = "path/to/params.json", file_type = "json")
#' # read_parameters_from_file(file_path = "path/to/params.yaml", file_type = "yaml")

read_params <- function(file_path, file_type = "yaml") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (file_type == "yaml") {
    # Read parameters from a YAML file
    params_data <- read_yaml(file_path)
  } else if (file_type == "json") {
    # Read parameters from a JSON file
    params_data <- fromJSON(file_path)
  } else if (file_type == "csv") {
    # Read parameters from a CSV file
    params_data <- read_csv(file_path)
  } else {
    stop("Invalid file type. Supported file types: 'csv', 'json', 'yaml'")
  }
  
  # Function to recursively evaluate R expressions within a list
  evaluate_r_expressions <- function(data) {
    if (is.list(data)) {
      for (key in names(data)) {
        data[[key]] <- evaluate_r_expressions(data[[key]])
      }
    } else if (is.character(data) && grepl("^r\\w*\\(", data)) {
      data <- eval(parse(text = data))
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
