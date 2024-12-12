app <- function() {
  ui <- fluidPage(
    useShinyjs(),
    includeScript(system.file("www/download.js", package = "bs")),
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js")
    ),
    sidebarLayout(
      sidebarPanel(
        div(
          class = "boxed-output",
          uiOutput("open_formula_editor_corr"),
          verbatimTextOutput("formula")
        ),
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
        ),
        uiOutput("Results")
      )
    )
  )

  server <- function(input, output, session) {
    dataSet <- reactiveValues(
      df = NULL, formula = NULL,
      backup_df = NULL, filter_col = NULL, filter_group = NULL
    )

    listResults <- reactiveValues(
      curr_data = NULL, curr_name = NULL,
      all_data = list(), all_names = list(),
      counter = 0
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
    observeEvent(input[["visualization_docu"]], {
      showModal(modalDialog(
        title = "Visualization",
        includeHTML(system.file("www/visualization1.html", package = "bs")),
        br(),
        renderImage(
          {
            list(
              src = system.file("www/DocuPlot.jpg", package = "bs"),
              contentType = "image/jpg",
              width = 650,
              height = 500,
              alt = "Basic Plot"
            )
          },
          deleteFile = FALSE
        ),
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
      file <- download(session, "/home/shiny/results") # NOTE: from COMELN
      df <- NULL
      df <- readData(file)
      print_req(is.data.frame(df), "File can not be used. Upload into R failed!")
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
        res <- try({
          download_file()
        })
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
          print_err(err)
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

    OperationEditorServer("OP", dataSet, listResults)
    corrServer("CORR", dataSet, listResults)
    visServer("VIS", dataSet, listResults)
    assServer("ASS", dataSet, listResults)
    testsServer("TESTS", dataSet, listResults)
    DoseResponseServer("DOSERESPONSE", dataSet, listResults)
    FormulaEditorServer("FO", dataSet)
    SplitByGroupServer("SG", dataSet)

    # Render results list
    output$Results <- renderUI({
      if (input$conditionedPanels == "DataWrangling" || input$conditionedPanels == "Dose Response analysis") {
        return(
          div(
            class = "var-box-output",
            h3(strong("The results are displayed in the other tabs"))
          )
        )
      }
      res <- listResults$all_data |> rev()
      if (length(res) == 0) {
        return()
      }
      res_ui_list <- lapply(names(res), function(name) {
        temp <- res[[name]]
        if (is.vector(temp)) {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            verbatimTextOutput(paste0("res_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (is.data.frame(temp)) {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            DTOutput(paste0("res_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "plot")) {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            plotOutput(paste0("res_", name), width = "100%", height = "800px"),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            verbatimTextOutput(paste0("res_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        }
      })
      download_stuff <- div(
        class = "var-box-output",
        h3(strong("Results")),
        p("The following list contains the results"),
        actionButton("download", "Save and exit"),
        textInput("user_filename", "Set filename", value = "")
      )
      do.call(tagList, list(download_stuff, res_ui_list))
    })

    # Show results
    observe({
      if (length(listResults$all_data) == 0) {
        return()
      }
      res <- listResults$all_data
      res_ui_list <- lapply(names(res), function(name) {
        observeEvent(res[[name]], {
          temp <- res[[name]]
          if (is.vector(temp)) {
            output[[paste0("res_", name)]] <- renderPrint(temp)
          } else if (is.data.frame(temp)) {
            output[[paste0("res_", name)]] <- renderDT(temp)
          } else if (inherits(temp, "plot")) {
            output[[paste0("res_", name)]] <- renderPlot(temp@p)
          } else if (inherits(temp, "doseResponse")) {
            message <- "Dose Response Analysis. Too large to display."
            output[[paste0("res_", name)]] <- renderPrint(message)
          } else {
            output[[paste0("res_", name)]] <- renderPrint(temp)
          }
        })
      })
      do.call(tagList, res_ui_list)
    })

    # Observe remove buttons
    observe({
      if (length(listResults$all_data) == 0) {
        return()
      }
      current_list <- listResults$all_data
      lapply(names(current_list), function(name) {
        observeEvent(input[[paste0("remove_res_", name)]],
          {
            current_list <- listResults$all_data
            current_list[[name]] <- NULL
            listResults$all_data <- current_list
          },
          ignoreInit = TRUE
        )
      })
    })

    observeEvent(input$download, {
      print_req(is_valid_filename(input$user_filename), "Defined filename is not valid")
      print_req(length(listResults$all_data) > 0, "No results to save")
      l <- listResults$all_data
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        print_req(check_filename_for_server(input$user_filename), "Defined filename does not have xlsx as extension")
        excelFile <- createExcelFile(l)
        upload(session, excelFile, new_name = input$user_filename)
      } else {
        print_req(check_filename_for_serverless(input$user_filename), "Defined filename does not have zip as extension")
        jsString <- createJSString(l)
        session$sendCustomMessage(
          type = "downloadZip",
          list(
            numberOfResults = length(jsString),
            FileContent = jsString,
            Filename = input$user_filename
          )
        )
      }
    })
  }

  return(list(ui = ui, server = server))
}
