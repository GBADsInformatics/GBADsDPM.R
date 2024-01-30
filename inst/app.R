# Load necessary libraries
library(shiny)
library(yaml)
library(DT)
library(mc2d)
library(truncnorm)
library(tools)

# Define functions

read_params <- function(file_path, file_type = "yaml") {
  # Error handling for incorrect file type
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  if (file_type != "yaml") {
    stop("Invalid file type. Supported file type: YAML")
  }

  # Read in YAML file
  params_data <- read_yaml(file_path)

  # Recursively evaluate R expressions within a list
  evaluate_r_expressions <- function(data, exclude_eval = character()) {
    if (is.list(data)) {
      for (key in names(data)) {
        data[[key]] <- evaluate_r_expressions(data[[key]], exclude_eval)
      }
    } else if (is.character(data)) {
      if (data %in% exclude_eval) {
        # Skip evaluation for strings in exclude_eval list
        return(data)
      }
      if (grepl("^r\\w*\\(", data)) {
        data <- eval(parse(text = data))
        # Evaluate rpert()
      } else if (identical(gsub("\\s+", "", data), as.character(parse(text = data)))) {
        data <- eval(parse(text = data))
      } else if (grepl("^\\d+\\s*[-+*/]\\s*\\d+$", data)) {
        data <- eval(parse(text = data))
      }
    }
    return(data)
  }

  # Define strings that should not be evaluated
  exclude_evaluation <- c("cattle",
                          "small ruminants",
                          "poultry")

  # Evaluate R expressions within the parameters data
  params_data <- evaluate_r_expressions(params_data, exclude_evaluation)

  # Return the parameters
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

  # special case if mu == mode

  if (mu == x_mode) {
    v <- (lambda / 2) + 1
  } else {
    v <- ((mu - x_min) * (2 * x_mode - x_min - x_max)) /
      ((x_mode - mu) * (x_max - x_min))
  }

  w <- (v * (x_max - mu)) / (mu - x_min)
  return (rbeta(n, v, w) * x_range + x_min)
}

# Define the Shiny app
ui <- fluidPage(
  fluidRow(
    column(2,
           img(src = "GBADs.png", width = 150, height = 150)
    ),
    column(10,
           titlePanel("The Dynamic Population Model (DPM)")
    )
  ),
  mainPanel(
    fileInput("file", "Choose YAML file", multiple = TRUE, accept = ".yaml"),
    checkboxInput("useRandomSeed", "Use random seed for reproducibility", FALSE),
    uiOutput("seedInput"),
    actionButton("readButton", "Read parameters"),
    tags$br(),  # Add vertical whitespace
    tags$br(),
    uiOutput("tables"),  # Display parameters in DataTables
    uiOutput("valuesNote"),  # Display rounding note
    uiOutput("roundingNote")  # Display rounding note
  )
)

server <- function(input, output, session) {
  # Reactive values to store parameters
  params <- reactiveValues(data = NULL)
  
  # Render seed input based on checkbox
  output$seedInput <- renderUI({
    if (input$useRandomSeed) {
      numericInput("seed", label = "Set seed value (can be an integer of any length)", value = NULL)
    } else {
      NULL
    }
  })
  
  observeEvent(input$readButton, {
    file_paths <- input$file$datapath
    file_names <- input$file$name
    
    # Set seed if provided
    if (input$useRandomSeed && !is.null(input$seed)) {
      set.seed(as.numeric(input$seed))
    }
    
    all_tables <- lapply(seq_along(file_paths), function(i) {
      file_path <- file_paths[i]
      file_name <- file_names[i]
      
      params$data <- read_params(file_path)
      
      # Round numeric values to 3 decimal places
      rounded_data <- lapply(params$data, function(x) if (is.numeric(x)) round(x, 3) else x)
      
      # Display only the first 6 values of each vector
      shortened_data <- lapply(rounded_data, function(x) if(is.vector(x)) head(x) else x)
      shortened_data <- t(shortened_data)
      
      datatable(shortened_data, caption = paste0("DPM data and parameters for ", file_path_sans_ext(file_name), " scenario"))
    })
    
    # Display multiple tables
    output$tables <- renderUI({
      do.call(tagList, all_tables)
    })
    
    # Display notes
    output$valuesNote <- renderUI({
      HTML("<b>Note 1</b>: Only the first six values are displayed in each table above.")
    })
    output$roundingNote <- renderUI({
      HTML("<b>Note 2</b>: Values in each table have been rounded to 3 decimal places of precision.")
    })
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
