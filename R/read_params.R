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

read_params <- function(file_path, file_type = "csv") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (file_type == "csv") {
    # Read parameters from a CSV file
    params_data <- read_csv(file_path)
  } else if (file_type == "json") {
    # Read parameters from a JSON file
    params_data <- fromJSON(file_path)
  } else if (file_type == "yaml") {
    # Read parameters from a YAML file
    params_data <- read_yaml(file_path)
  } else {
    stop("Invalid file type. Supported file types: 'csv', 'json', 'yaml'")
  }
  
  # Loop through the names and values and set the parameters accordingly
  for (parameter in names(params_data)) {
    value <- params_data[[parameter]]
    
    # Set the parameter value using assign()
    assign(parameter, value, envir = parent.frame())
  }
}
