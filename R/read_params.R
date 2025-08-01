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

# read_params <- function(file_path, file_type = "yaml") {
#   if (!file.exists(file_path)) {
#     stop("File not found: ", file_path)
#   }
#   
#   if (file_type != "yaml") {
#     stop("Invalid file type. Supported file type: YAML")
#   }
#   
#   params_data <- read_yaml(file_path)
#   
#   # excluded_fields <- c("Type_of_scenario",
#   #                      "scenario_name",
#   #                      "currency_used")
#   # 
#   # params_data <- params_data[!names(params_data) %in% excluded_fields]
# 
#   evaluate_r_expressions <- function(data, exclude_eval = character()) {
#     if (is.list(data)) {
#       for (key in names(data)) {
#         data[[key]] <- evaluate_r_expressions(data[[key]], exclude_eval)
#       }
#     } else if (is.character(data)) {
#       if (data %in% exclude_eval) {
#         return(data)
#       }
#       if (grepl("^r\\w*\\(", data)) {
#         data <- eval(parse(text = data))
#       } else if (identical(gsub("\\s+", "", data), as.character(parse(text = data)))) {
#         data <- eval(parse(text = data))
#       } else if (grepl("^\\d+\\s*[-+*/]\\s*\\d+$", data)) {
#         data <- eval(parse(text = data))
#       }
#       
#     }
#     return(data)
#   }
#   
#   exclude_evaluation <- c("cattle", 
#                           "smallruminants", 
#                           "poultry",
#                           "swine",
#                           "equids")
#   
#   params_data <- evaluate_r_expressions(params_data, exclude_evaluation)
#   
#   for (parameter in names(params_data)) {
#     value <- params_data[[parameter]]
#     
#     assign(parameter, value, envir = .GlobalEnv)
#   }
# }



read_params <- function(file_path, file_type = "yaml") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (file_type != "yaml") {
    stop("Invalid file type. Supported file type: YAML")
  }
  
  params_data <- read_yaml(file_path)
  
  # Recursively evaluate expressions where appropriate
  evaluate_r_expressions <- function(data, exclude_eval = character()) {
    if (is.list(data)) {
      for (key in names(data)) {
        data[[key]] <- evaluate_r_expressions(data[[key]], exclude_eval)
      }
    } else if (is.character(data)) {
      if (data %in% exclude_eval) {
        return(data)
      }
      # Only evaluate if it's a supported R function call like rpert(...) or runif(...)
      if (grepl("^r\\w*\\(", data)) {
        data <- eval(parse(text = data))
      }
    }
    return(data)
  }
  
  # Values that should not be evaluated (e.g., species names)
  exclude_evaluation <- c("cattle", 
                          "smallruminants", 
                          "poultry",
                          "swine",
                          "equids")
  
  # Evaluate any embedded R expressions like rpert(...)
  params_data <- evaluate_r_expressions(params_data, exclude_evaluation)
  
  # Assign each parameter to the global environment
  for (parameter in names(params_data)) {
    value <- params_data[[parameter]]
    assign(parameter, value, envir = .GlobalEnv)
  }
}
