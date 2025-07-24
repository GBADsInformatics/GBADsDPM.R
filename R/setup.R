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
      model_output <- run_compartmental_model()
      base_file_name <- tools::file_path_sans_ext(basename(file_name))
      
      if (!is.null(model_output$cumulative)) {
        out_file_cumulative <- file.path(file_path, paste0(base_file_name, "_cumulative_total_results.csv"))
        write.csv(model_output$cumulative, file = out_file_cumulative, row.names = TRUE)
      }
      
      if (!is.null(model_output$summary)) {
        out_file_summary <- file.path(file_path, paste0(base_file_name, "_summary_results.csv"))
        write.csv(model_output$summary, file = out_file_summary, row.names = TRUE)
      }
      
    }
  } else {
    
    # Parallelization
    
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
  
    out <- foreach (file_name = 1:length(file_names), .packages = "GBADsDPM", .combine = ) %dopar% {
      params <- read_params(file_path = file_name, file_type = file_type)
      model_output <- run_compartmental_model()
      
      if (!is.null(model_output$cumulative)) {
        base_file_name <- tools::file_path_sans_ext(basename(file_name))
        out_file_cum <- file.path(file_path, paste0(base_file_name, "_cumulative_total_results.csv"))
        write.csv(model_output$cumulative, file = out_file_cum, row.names = TRUE)
      }
      
      if (!is.null(model_output$summary)) {
        base_file_name <- tools::file_path_sans_ext(basename(file_name))
        out_file_sum <- file.path(file_path, paste0(base_file_name, "_summary_results.csv"))
        write.csv(model_output$summary, file = out_file_sum, row.names = TRUE)
      }
      
    }
    
  }
}

