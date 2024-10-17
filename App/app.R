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
library(agricolae)
library(drc)
library(cowplot)
library(patchwork)
library(httr)
library(readxl)
library(openxlsx)
library(COMELN)
library(openssl)
library(jose)
library(png)
library(ggpmisc)
library(R6)
library(drc)
library(patchwork)



source("check_ast.R")
source("utils.R")
source("plottingInternally.R")
source("lc50.r")
source("correlation.R")
source("visualisation.R")
source("assumption.R")
source("statisticalTests.R")
source("DoseResponse.R")

ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.conditionedPanels == 'Data'",
        uiOutput("conditional_data_ui"),
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
      ),
      conditionalPanel(
        condition = "input.conditionedPanels == 'Tests'",
        testsSidebarUI("TESTS")
      ),
      conditionalPanel(
        condition = "input.conditionedPanels == 'Dose Response analysis'",
        DoseResponseSidebarUI("DOSERESPONSE")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data",
          DTOutput("df")
        ),
        tabPanel(
          "Correlation",
          corrUI("CORR")
        ),
        tabPanel(
          "Visualisation",
          visUI("VIS")
        ),
        tabPanel(
          "Assumption",
          assUI("ASS")
        ),
        tabPanel(
          "Tests",
          testsUI("TESTS")
        ),
        tabPanel(
          "Dose Response analysis",
          DoseResponseUI("DOSERESPONSE")
        ),
        id = "conditionedPanels"
      )
    )
  )
)

server <- function(input, output, session) {
  dataSet <- reactiveValues(df = NULL)

  output$conditional_data_ui <- renderUI({
    if (Sys.getenv("RUN_MODE") == "BROWSER") {
      conditionalPanel(
        condition = "input.conditionedPanels == 'Data'",
        fileInput("file", "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )
      )
    }
  })

  download_file <- reactive({
    file <- COMELN::download(session, "/home/shiny/results")
    upload <- function(path) {
      stopifnot(is.character(path))
      df <- NULL
      df <- try(as.data.frame(read_excel(path, col_names = TRUE)), silent = TRUE)
      if (class(df) == "try-error") {
        # identify seperator
        line <- readLines(path, n = 1)
        semicolon <- grepl(";", line)
        comma <- grepl(",", line)
        tab <- grepl("\t", line)
        seperator <- NULL
        if (semicolon == TRUE) {
          seperator <- ";"
        } else if (comma == TRUE) {
          seperator <- ","
        } else if (tab == TRUE) {
          seperator <- "\t"
        } else {
          return("error")
        }
        df <- try(read.csv(path, header = TRUE, sep = seperator))
        if (class(df) == "try-error") {
          return("error")
        }
      } else {
        f <- function(x) {
          options(warn = -1)
          x <- as.numeric(x)
          options(warn = 0)
          x <- x[!is.na(x)]
          length(x) > 0
        }
        check <- apply(df, 2, f)
        conv <- function(a, b) {
          if (a == TRUE) {
            return(as.numeric(b))
          }
          return(b)
        }
        df <- Map(conv, check, df)
        df <- data.frame(df)
      }
      return(df)
    }
    df <- NULL
    df <- upload(file)
    if (is.data.frame(df)) {
      var$df <- df
    } else {
      showNotification("File can not be used. Upload into R failed!", duration = 0)
    }
    tryCatch(
      {
        system(paste("rm -r ", file))
      },
      warning = function(warn) {
        showNotification(paste("A warning occurred: ", conditionMessage(warn)), duration = 0)
      },
      error = function(err) {
        showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
      }
    )
    req(is.data.frame(df))
    return(df)
  })


  output$df <- renderDT({
    if (Sys.getenv("RUN_MODE") == "BROWSER") {
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
    } else if (Sys.getenv("RUN_MODE") == "SERVER") {
      isolate({
        dataSet$df <- download_file()
      })
      datatable(dataSet$df, options = list(pageLength = 10))
    }
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
      stopifnot(get_ast(str2lang(input$keepVar)) != "Error")
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
      stopifnot(get_ast(str2lang(input$value)) != "Error")
      stopifnot(get_ast(str2lang(input$name)) != "Error")
      dataSet$df <- unstackDF(dataSet$df, input$name, input$value)
    })
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }
    output$df <- renderDT(dataSet$df)
    output$mod_error <- renderText(err)
    return(df)
  })

  listResults <- reactiveValues(
    curr_data = NULL, curr_name = NULL,
    all_data = list(), all_names = list()
  )
  corrServer("CORR", dataSet, listResults)
  visServer("VIS", dataSet, listResults)
  assServer("ASS", dataSet, listResults)
  testsServer("TESTS", dataSet, listResults)
  DoseResponseServer("DOSERESPONSE", dataSet, listResults)
}

shinyApp(ui, server)
