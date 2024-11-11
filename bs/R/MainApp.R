source("loadLibraries.R")
source("check_ast.R")
source("utils.R")
source("plottingInternally.R")
source("lc50.r")
source("correlation.R")
source("visualisation.R")
source("assumption.R")
source("statisticalTests.R")
source("DoseResponse.R")
source("OperationsModule.R")
source("FormulaModule.R")
source("SplitByGroup.R")
source("DiagnosticPlots.R")

ui <- fluidPage(
  useShinyjs(),
  includeScript("www/download.js"),
  sidebarLayout(
    sidebarPanel(
      div(
        conditionalPanel(
          condition = "input.conditionedPanels == 'Data'",
          div(
            style = "position: relative",
            actionButton(
              "data_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          uiOutput("conditional_data_ui"),
          tags$hr()
        ),
        conditionalPanel(
          condition = "input.conditionedPanels == 'DataWrangling'",
          div(
            style = "position: relative",
            actionButton(
              "datawrangling_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          OperatorEditorSidebar("OP")
        ),
        conditionalPanel(
          condition = "input.conditionedPanels == 'Correlation'",
          div(
            style = "position: relative",
            actionButton(
              "corr_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          corrSidebarUI("CORR")
        ),
        conditionalPanel(
          condition = "input.conditionedPanels == 'Visualisation'",
          div(
            style = "position: relative;",
            actionButton(
              "visualization_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          visSidebarUI("VIS")
        ),
        conditionalPanel(
          condition = "input.conditionedPanels == 'Assumption'",
          div(
            style = "position: relative",
            actionButton(
              "ass_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          assSidebarUI("ASS")
        ),
        conditionalPanel(
          condition = "input.conditionedPanels == 'Tests'",
          div(
            style = "position: relative",
            actionButton(
              "test_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          testsSidebarUI("TESTS")
        ),
        conditionalPanel(
          condition = "input.conditionedPanels == 'Dose Response analysis'",
          div(
            style = "position: relative;",
            actionButton(
              "doseresponse_docu",
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          DoseResponseSidebarUI("DOSERESPONSE")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data",
          DTOutput("df")
        ),
        tabPanel(
          "DataWrangling",
          OperatorEditorUI("OP")
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
  dataSet <- reactiveValues(
    df = NULL, formula = NULL,
    backup_df = NULL, filter_col = NULL, filter_group = NULL
  )

  # docu data
  observeEvent(input[["data_docu"]], {
    showModal(modalDialog(
      title = "Example Dataframe",
      includeHTML("www/data.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input[["datawrangling_docu"]], {
    showModal(modalDialog(
      title = "Data wrangling",
      includeHTML("www/operations.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input[["corr_docu"]], {
    showModal(modalDialog(
      title = "Correlation",
      includeHTML("www/correlation.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input[["ass_docu"]], {
    showModal(modalDialog(
      title = "Testing assumptions",
      includeHTML("www/assumptions.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input[["test_docu"]], {
    showModal(modalDialog(
      title = "Statistical tests",
      includeHTML("www/tests.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # docu dose response
  observeEvent(input[["doseresponse_docu"]], {
    showModal(modalDialog(
      title = "Doseresponse analysis",
      includeHTML("www/doseresponse.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # docu visualisation
  observeEvent(input[["visualization_docu"]], {
    showModal(modalDialog(
      title = "Visualization",
      includeHTML("www/visualization.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # docu formula editor
  observeEvent(input[["FO-formula_docu"]], {
    showModal(modalDialog(
      title = "Defining the formula",
      includeHTML("www/formula.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })

  output$conditional_data_ui <- renderUI({
    if (Sys.getenv("RUN_MODE") != "SERVER") {
      res <- conditionalPanel(
        condition = "input.conditionedPanels == 'Data'",
        fileInput("file", "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )
      )
      return(res)
    }
  })

  download_file <- reactive({
    file <- COMELN::download(session, "/home/shiny/results")
    upload <- function(path) {
      stopifnot(is.character(path))
      df <- NULL
      df <- try(as.data.frame(readxl::read_excel(
        path,
        col_names = TRUE
      )), silent = TRUE)
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
    if (!is.data.frame(df)) {
      showNotification("File can not be used. Upload into R failed!", duration = 0)
    }
    tryCatch(
      {
        unlink(file)
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
    if (Sys.getenv("RUN_MODE") == "SERVER") {
      res <- try({download_file()})
      if (inherits(res, "try-error")) {
        stop(attributes(res)$condition)
      } else {
        res <- create_r_names(res)
        dataSet$df <- res
      }
      datatable(dataSet$df, options = list(pageLength = 10))
    } else {
      req(input$file)
      df <- try(read.csv(input$file$datapath))
      if (inherits(df, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        showNotification(err)
        return(NULL)
      }
      df <- create_r_names(df)
      dataSet$df <- df
      req(!is.na(dataSet$df))
      datatable(dataSet$df, options = list(pageLength = 10))
    }
  })

  observe({
    req(!is.null(dataSet$df))
    req(is.data.frame(dataSet$df))
    output$df <- renderDT(
      datatable(dataSet$df, options = list(pageLength = 10))
    )
  })

  listResults <- reactiveValues(
    curr_data = NULL, curr_name = NULL,
    all_data = list(), all_names = list()
  )
  OperationEditorServer("OP", dataSet)
  corrServer("CORR", dataSet, listResults)
  visServer("VIS", dataSet, listResults)
  assServer("ASS", dataSet, listResults)
  testsServer("TESTS", dataSet, listResults)
  DoseResponseServer("DOSERESPONSE", dataSet, listResults)
  FormulaEditorServer("FO", dataSet)
  SplitByGroupServer("SG", dataSet)
}
