library(shiny)
library(yaml)
library(DT)
library(mc2d)
library(truncnorm)
library(tools)
library(shinyjs)

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
                          "small ruminants",
                          "poultry")
  
  params_data <- evaluate_r_expressions(params_data, exclude_evaluation)
  
  return(params_data)
}

rpert <- function(n, x_min, x_max, x_mode, lambda = 4) {
  if (x_min > x_max || x_mode > x_max || x_mode < x_min) {
    stop("invalid parameters")
  }
  x_range <- x_max - x_min
  if (x_range == 0) {
    return(rep(x_min, n))
  }
  
  mu <- (x_min + x_max + lambda * x_mode) / (lambda + 2)
  
  if (mu == x_mode) {
    v <- (lambda / 2) + 1
  } else {
    v <- ((mu - x_min) * (2 * x_mode - x_min - x_max)) /
      ((x_mode - mu) * x_range)
  }
  
  w <- (v * (x_max - mu)) / (mu - x_min)
  return (rbeta(n, v, w) * x_range + x_min)
}