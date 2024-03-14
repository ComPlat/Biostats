library(shiny)
library(DT)
library(bslib)
library(broom)
library(utils)
library(ggplot2)

source("check_ast.R")
source("correlation.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.conditionedPanels == 'Data'",
        fileInput("file", "Choose CSV File",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
        ),
        textInput("op", "Operations", value = "var / 1000"),
        textInput("new_col", "Name of new variable", value = "var"),
        actionButton("mod", "Modify"),
        verbatimTextOutput("mod_error"),
        tags$hr(),
        helpText("Please upload a CSV file.")
      ),
      conditionalPanel(
        condition = "input.conditionedPanels == 'Correlation'",
        corrSidebarUI("CORR")
      )
    ),  
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data",
            DTOutput("df")
        ),
        
        tabPanel("Correlation",
            corrUI("CORR")
        ),
        
        
        id = "conditionedPanels"   
      )
    )
    
  )
)

server <- function(input, output) {
  data <- reactiveValues(df = NA)
  output$df <- renderDT({
    req(input$file)
    df <- try(read.csv(input$file$datapath))
    if (inherits(df, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
      showNotification(err)
      return(NULL)
    } 
    data$df <- df
    req(!is.na(data$df))
    datatable(data$df, options = list(pageLength = 10)) 
  })

  observeEvent(input$mod, {
    req(!is.null(data$df))
    req(is.data.frame(data$df))
    req(input$op)
    req(input$new_col)
    dt <- data$df
    op <- input$op
    new_col <- input$new_col
    new <- NULL
    err <- NULL
    e <- try({
      ast <- get_ast(str2lang(op))
      ast <- ast[[length(ast)]]
    })
    if (e == "Error") {
      showNotification("Found unallowed function")
      return()
    } else if (inherits(e, "try-error")) {
      showNotification(e)
      return()
    }
    e <- try(
      new <- with(dt, eval(parse(text = op)))
    )
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    } else {
      data$df[, new_col] <- new
    }
    output$df <- renderDT(data$df)
    output$mod_error <- renderText(err)  
    return(df)
  })
  
  listResults <- reactiveValues(curr_data = NULL, curr_name = NULL,
                                all_data = list(), all_names = list())
  listResults <- corrServer("CORR", data$df, listResults)
  
  
}

shinyApp(ui, server)