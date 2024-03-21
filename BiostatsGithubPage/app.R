library(shiny)
library(DT)
library(bslib)
library(broom)
library(utils)
library(ggplot2)
library(base64enc)
library(shinyjs)
library(mgcv)
library(RColorBrewer)
library(tidyr)
library(purrr)

source("check_ast.R")
source("utils.R")
source("plottingInternally.R")
source("correlation.R")
source("visualisation.R")
source("assumption.R")

ui <- fluidPage(
  useShinyjs(),
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
        tags$hr(),
        textInput("keepVar", "const variable"),
        actionButton("pivotLonger", "conversion to long format"),
        tags$hr(),
        textInput("name", "name column"),
        textInput("value", "value column"),
        actionButton("pivotWider", "convert to wide format"),
        verbatimTextOutput("mod_error"),
        tags$hr(),
        helpText("Please upload a CSV file.")
      ),
      conditionalPanel(
        condition = "input.conditionedPanels == 'Correlation'",
        corrSidebarUI("CORR")
      ),
      conditionalPanel(
        condition = "input.conditionedPanels == 'Visualisation'",
        visSidebarUI("VIS")
      ),
      conditionalPanel(
        condition = "input.conditionedPanels == 'Assumption'",
        assSidebarUI("ASS")
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
        
        tabPanel("Visualisation",
            visUI("VIS")
        ),

        tabPanel("Assumption",
            assUI("ASS")
        ),
        
        id = "conditionedPanels"   
      )
    )
    
  )
)

server <- function(input, output) {
  dataSet <- reactiveValues(df = NULL)

  output$df <- renderDT({
    req(input$file)
    df <- try(read.csv(input$file$datapath))
    if (inherits(df, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
      showNotification(err)
      return(NULL)
    } 
    dataSet$df <- df
    req(!is.na(dataSet$df))
    datatable(dataSet$df, options = list(pageLength = 10)) 
  })

  observeEvent(input$mod, {
    req(!is.null(dataSet$df))
    req(is.data.frame(dataSet$df))
    req(input$op)
    req(input$new_col)
    dt <- dataSet$df
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
    e <- try({
      new <- with(dt, eval(parse(text = op)))
      dataSet$df[, new_col] <- new
    })
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    } 
    output$df <- renderDT(dataSet$df)
    output$mod_error <- renderText(err)  
    return(df)
  })

  observeEvent(input$pivotLonger, {
    req(!is.null(dataSet$df))
    req(input$keepVar)
    err <- NULL
    e <- try({
      dataSet$df <- stackDF(dataSet$df, input$keepVar)
    })
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }
    output$df <- renderDT(dataSet$df)
    output$mod_error <- renderText(err)  
    return(df)
  })

  observeEvent(input$pivotWider, {
    req(!is.null(dataSet$df))
    req(input$name)
    req(input$value)
    err <- NULL
    e <- try({
      dataSet$df <- unstackDF(dataSet$df, input$name, input$value)
    })
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }
    output$df <- renderDT(dataSet$df)
    output$mod_error <- renderText(err)  
    return(df)
  })
  
  listResults <- reactiveValues(curr_data = NULL, curr_name = NULL,
                                all_data = list(), all_names = list())
  corrServer("CORR", dataSet, listResults)
  visServer("VIS", dataSet, listResults)
  assServer("ASS", dataSet, listResults)
  
}

shinyApp(ui, server)