#' @title
#' Import DPM/AHLE parameter file 

#' @description
#' Read in AHLE parameter file in YAML format
#' 
#' @param file_path Relative or absolute path to file
#' @param file_type File type (Currently only YAML is supported)
#' 
#' @example
#' # Read_parameters_from_file(file_path = "path/to/params.yaml", file_type = "yaml")

read_params <- function(file_path, file_type = "yaml") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (file_type != "yaml") {
    stop("Invalid file type. Supported file type: YAML")
  }
  
  params_data <- read_yaml(file_path)
  
  evaluate_r_expressions <- function(data, exclude_eval = character()) {
    if (is.list(data)) {
      for (key in names(data)) {
        data[[key]] <- evaluate_r_expressions(data[[key]], exclude_eval)
      }
    } else if (is.character(data)) {
      if (data %in% exclude_eval) {
        return(data)
      }
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
  
  exclude_evaluation <- c("cattle", 
                          "smallruminants", 
                          "poultry",
                          "swine",
                          "equids")
  
  params_data <- evaluate_r_expressions(params_data, exclude_evaluation)
  
  for (parameter in names(params_data)) {
    value <- params_data[[parameter]]
    
    assign(parameter, value, envir = .GlobalEnv)
  }
}