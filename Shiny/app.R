# Shiny app to set up DPM model

source("Functions/globals.R")
source("Functions/run_compartmental_model.R")

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(2,
           img(src = "GBADs.png", width = 150, height = 150, onclick = "openWebsite()")
    ),
    column(10,
           titlePanel("The Dynamic Population Model (DPM)")
    )
  ),
  mainPanel(
    fileInput("file", "Choose YAML file(s)", multiple = TRUE, accept = ".yaml"),
    selectInput("selectedFiles", "Select File(s):", choices = NULL, multiple = TRUE),
    checkboxInput("useRandomSeed", "Use random seed for reproducibility", FALSE),
    uiOutput("seedInput"),
    actionButton("readButton", "Read parameters"),
    actionButton("resetButton", "Reset"),  # Reset button
    actionButton("runCompartmentalModelButton", "Run model"),  # Button to trigger compartmental model simulation
    tags$br(),  # Add vertical whitespace
    tags$br(),
    uiOutput("tables"),  # Display parameters in DataTables
    uiOutput("valuesNote"),  # Display rounding note
    uiOutput("roundingNote"),  # Display rounding note
    DTOutput("modelResultsTable"),  # Display table for model results
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

  observeEvent(input$file, {
    updateSelectInput(session, "selectedFiles", choices = input$file$name)
  })

  observeEvent(input$readButton, {
    file_paths <- input$file$datapath
    file_names <- input$file$name
    updateSelectInput(session, "selectedFiles", choices = file_names, selected = NULL)

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

  # Add an action button to reset fileInput, selectInput, and displayed tables
  observeEvent(input$resetButton, {
    output$tables <- renderUI(NULL)
    output$valuesNote <- renderUI(NULL)  # Clear the values note
    output$roundingNote <- renderUI(NULL)  # Clear the rounding note
  })

  observeEvent(input$runCompartmentalModelButton, {
    run_compartmental_model()
    
    showModal(modalDialog(
      title = "Simulation Complete",
      "Compartmental model simulation has been completed."
    ))
  })

  runjs("
    openWebsite = function() {
      window.open('https://www.gbadske.org', '_blank');
    }
  ")

}

shinyApp(ui, server)