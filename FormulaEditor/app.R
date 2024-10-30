source("OperationsModule.R")
source("FormulaModule.R")
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .modal-dialog {
      width: 80%;  /* Set the width */
      max-width: none;  /* Remove max width limit */
      }
      "))
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,
          .csv"
        )
      ),
      uiOutput("open_formula_editor_ui"),
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Data",
          tableOutput("head")
        ),
        tabPanel(
          "DataWrangling",
          OperatorEditorUI("OP")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  r_vals <- reactiveValues(df = NULL, formula = NULL)
  data <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    df <- read.csv(infile$datapath, header = TRUE)
    # TODO: add the conversion to factor also in bs
    df <- lapply(df, function(x) {
      if (is.character(x)) {
        return(as.factor(x)) # TODO: keep original names
      }
      return(x)
    })
    df <- do.call(cbind, df)
    df <- as.data.frame(df)
    return(df)
  })

  output$head <- renderTable({
    r_vals$df <- data()
    head(r_vals$df, 10)
  })

  observe({
    req(is.data.frame(r_vals$df))
    output$head <- renderTable({
      head(r_vals$df, 10)
    })
  })

  # UI output for the formula editor
  output$open_formula_editor_ui <- renderUI({
    actionButton("open_formula_editor",
      "Open formula editor",
      title = "Open the formula editor to create or modify a formula",
      disabled = is.null(r_vals$df) || !is.data.frame(r_vals$df)
    )
  })

  # Make "DataWrangling" tab clickable only when r_vals$df is a data frame
  observe({
    if (!is.null(r_vals$df) && is.data.frame(r_vals$df)) {
      showTab(inputId = "tabs", target = "DataWrangling")
    } else {
      hideTab(inputId = "tabs", target = "DataWrangling")
    }
  })

  OperationEditorServer("OP", r_vals)
  FormulaEditorServer("FO", r_vals)

  observeEvent(input$open_formula_editor, {
    showModal(modalDialog(
      title = "FormulaEditor",
      FormulaEditorUI("FO"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
}

shinyApp(ui = ui, server = server)
