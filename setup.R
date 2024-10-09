#' @title
#' Set up and run DPM/AHLE scenarios

#' @description
#' Read in a DPM/AHLE parameter file in YAML format and run compartmental model. 
#' 
#' @param file_path A path to a directory containing YAML files to be analyzed
#' @param seed_value An integer value of any length for reproducibility
#' @parallel A logical (TRUE/FALSE) value indicating whether parallelization is 
#' desired. By default, parallel = FALSE. 
#' 
#' @example
#' # setup(file_path = file_path, seed_value = NULL)

setup <- function(file_path, seed_value = NULL, parallel = FALSE) {
  file_names <- list.files(file_path, pattern = "*.yaml", full.names = TRUE)
  file_type <- "yaml"
  
  set.seed(seed_value)
  
  if (parallel == FALSE) { # sequential
    for (file_name in file_names) {
      params <- read_params(file_path = file_name, file_type = file_type)
      df <- run_compartmental_model(seed_value = seed_value)
      base_file_name <- file_path_sans_ext(basename(file_name))
      output_file <- file.path(file_path, paste0(base_file_name, "_final_month.csv"))
      write.csv(df, file = output_file, row.names = TRUE)
    }
  } else {
    
    # Parallelization
    
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
  
    foreach (file_name = 1:length(file_names), .packages = c("mc2d", "truncnorm", "yaml", "rstudioapi", "tools")) %dopar% {
      params <- read_params(file_path = file_name, file_type = file_type)
      df <- run_compartmental_model(seed_value = seed_value)
      base_file_name <- file_path_sans_ext(basename(file_name))
      output_file <- file.path(file_path, paste0(base_file_name, "_final_month.csv"))
      write.csv(df, file = output_file, row.names = TRUE)
    }
    
  }
}

