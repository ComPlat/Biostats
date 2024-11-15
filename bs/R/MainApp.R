app <- function() {

  ui <- fluidPage(
    useShinyjs(),
    includeScript(system.file("www/download.js", package = "bs")),
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
            "Visualisation",
            visUI("VIS")
          ),
          tabPanel(
            "Assumption",
            assUI("ASS")
          ),
          tabPanel(
            "Correlation",
            corrUI("CORR")
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
        includeHTML(system.file("www/data.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input[["datawrangling_docu"]], {
      showModal(modalDialog(
        title = "Data wrangling",
        includeHTML(system.file("www/operations.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input[["corr_docu"]], {
      showModal(modalDialog(
        title = "Correlation",
        includeHTML(system.file("www/data.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input[["ass_docu"]], {
      showModal(modalDialog(
        title = "Testing assumptions",
        includeHTML(system.file("www/assumptions.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input[["test_docu"]], {
      showModal(modalDialog(
        title = "Statistical tests",
        includeHTML(system.file("www/tests.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    # docu dose response
    observeEvent(input[["doseresponse_docu"]], {
      showModal(modalDialog(
        title = "Doseresponse analysis",
        includeHTML(system.file("www/doseresponse.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    # docu visualisation
    # TODO: put html strings in own html files
    observeEvent(input[["visualization_docu"]], {
      showModal(modalDialog(
        title = "Visualization",
        includeHTML(system.file("www/visualization1.html", package = "bs")),
        br(),
        renderImage({
          list(src = system.file("www/DocuPlot.jpg", package = "bs"),
            contentType = 'image/jpg',
            width = 650,
            height = 500,
            alt = "Basic Plot"
          )
        }, deleteFile = FALSE),
        br(),
        br(),
        br(),
        br(),
        br(),
        includeHTML(system.file("www/visualization2.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })
    # docu formula editor
    observeEvent(input[["FO-formula_docu"]], {
      showModal(modalDialog(
        title = "Defining the formula",
        includeHTML(system.file("www/formula.html", package = "bs")),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })
    # docu split by group
    observeEvent(input[["SG-split_docu"]], {
      showModal(modalDialog(
        title = "Subsetting the dataset",
        includeHTML(system.file("www/SplitData.html", package = "bs")),
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
      df <- NULL
      df <- readData(file)
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
        df <- try(readData(input$file$datapath))
        if (inherits(df, "try-error")) {
          err <- conditionMessage(attr(df, "condition"))
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
    OperationEditorServer("OP", dataSet, listResults)
    corrServer("CORR", dataSet, listResults)
    visServer("VIS", dataSet, listResults)
    assServer("ASS", dataSet, listResults)
    testsServer("TESTS", dataSet, listResults)
    DoseResponseServer("DOSERESPONSE", dataSet, listResults)
    FormulaEditorServer("FO", dataSet)
    SplitByGroupServer("SG", dataSet)
  }

  return(list(ui = ui, server = server))

}
