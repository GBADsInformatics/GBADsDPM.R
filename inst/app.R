library(shiny)
library(yaml)
library(DT)
library(mc2d)
library(truncnorm)
library(tools)

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
  params <- reactiveValues(data = NULL)

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

    if (input$useRandomSeed && !is.null(input$seed)) {
      set.seed(as.numeric(input$seed))
    }

    all_tables <- lapply(seq_along(file_paths), function(i) {
      file_path <- file_paths[i]
      file_name <- file_names[i]

      params$data <- read_params(file_path)

      rounded_data <- lapply(params$data, function(x) if (is.numeric(x)) round(x, 3) else x)

      shortened_data <- lapply(rounded_data, function(x) if(is.vector(x)) head(x) else x)
      shortened_data <- t(shortened_data)

      datatable(shortened_data, caption = paste0("DPM data and parameters for ", file_path_sans_ext(file_name), " scenario"))
    })

    output$tables <- renderUI({
      do.call(tagList, all_tables)
    })

    output$valuesNote <- renderUI({
      HTML("<b>Note 1</b>: Only the first six values are displayed in each table above.")
    })
    output$roundingNote <- renderUI({
      HTML("<b>Note 2</b>: Values in each table have been rounded to 3 decimal places of precision.")
    })
  })

}

shinyApp(ui, server)

