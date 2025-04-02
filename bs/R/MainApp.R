js_scripts <- function() {
  if (Sys.getenv("RUN_MODE") == "BROWSER") {
    tagList(
      includeScript("www/FileSaver.min.js"),
      includeScript("www/html2canvas.min.js"),
      includeScript("www/jszip.min.js"),
      includeScript("www/download.js")
    )
  } else {
    tagList(
      includeScript(system.file("www/FileSaver.min.js", package = "bs")),
      includeScript(system.file("www/html2canvas.min.js", package = "bs")),
      includeScript(system.file("www/jszip.min.js", package = "bs")),
      includeScript(system.file("www/download.js", package = "bs"))
    )
  }
}

upload_ui_field <- function() {
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
}

app <- function() {
  js <- js_scripts()
  uploadUIField <- upload_ui_field()
  ui <- fluidPage(
    useShinyjs(),
    js,
    # includeScript(system.file("www/FileSaver.min.js", package = "bs")),
    # includeScript(system.file("www/html2canvas.min.js", package = "bs")),
    # includeScript(system.file("www/jszip.min.js", package = "bs")),
    # includeScript(system.file("www/download.js", package = "bs")),
    tags$head(
      # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      # tags$script(src tags$script(src = "js/jszip.min.js"),
      tags$style(HTML("
        .boxed-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        }
        .add-button {
        position: relative;
        padding-right: 20px;
        }
        .add-button::after {
        content: '\\2295';
        position: absolute;
        top: 1.1px;
        right: 5px;
        font-size: 16px;
        font-weight: bold;
        color: #900C3F;
        width: 15px;
        height: 15px;
        display: flex;
        justify-content: center;
        align-items: center;
        }
        .model {
        background-color: #f8f9fa;
        padding: 15px;
        border: 2px solid #c8c8c8;
        border-radius: 5px;
        margin-top: 10px;
        }
        .title {
        font-size: 14px;
        font-weight: bold;
        margin-bottom: 10px;
        color: #333;
        }
        .create_button{
        background-color: #04AA6D; /* Green */
        border: none;
        color: black;
        padding: 15px 32px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        }
        .var-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        display: inline-block;
        width: auto;
        }
        .var-box-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        }
        .nav-tabs > li > a {
          text-decoration: none;
          color: #900C3F;
          font-weight: bold;
          margin-right: 15px;
        }
        .nav-tabs > li > a:hover {
        text-decoration: underline;
        color: #333;
        }
        "))
    ),
    sidebarLayout(
      sidebarPanel(
        div(
          style = "position: relative",
          actionButton(
            "docu",
            label = NULL,
            icon = icon("question-circle")
          )
        ),
        uiOutput("open_formula_editor_main"),
        uiOutput("formulaUI"),
        br(),
        uiOutput("open_split_by_group"),
        uiOutput("data_splitted"),
        verbatimTextOutput("applied_filter"),
        br(),
        div(
          conditionalPanel(
            condition = "input.conditionedPanels == 'Data'",
            # uiOutput("conditional_data_ui"),
            uploadUIField,
            tags$hr()
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'DataWrangling'",
            OperatorEditorSidebar("OP")
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
            condition = "input.conditionedPanels == 'Correlation'",
            corrSidebarUI("CORR")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Tests'",
            testsSidebarUI("TESTS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Dose Response analysis'",
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
      history = list(),
      counter = 0
    )

    # docu
    observeEvent(input[["docu"]], {
      path <- ""
      if (input$conditionedPanels == "Data") {
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path <- "./www/data.html"
        } else {
          path <- system.file("www/data.html", package = "bs")
        }
        showModal(modalDialog(
          title = "Example Dataframe",
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (input$conditionedPanels == "DataWrangling") {
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path <- "./www/operations.html"
        } else {
          path <- system.file("www/operations.html", package = "bs")
        }
        showModal(modalDialog(
          title = "Data wrangling",
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (input$conditionedPanels == "Visualisation") {
        path1 <- ""
        path2 <- ""
        plot_path <- ""
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path1 <- "./www/visualization1.html"
          path2 <- "./www/visualization2.html"
          plot_path <- "www/DocuPlot.jpg"
        } else {
          path1 <- system.file("www/visualization1.html", package = "bs")
          path2 <- system.file("www/visualization2.html", package = "bs")
          plot_path <- system.file("www/DocuPlot.jpg", package = "bs")
        }
        showModal(modalDialog(
          title = "Visualization",
          includeHTML(path1),
          br(),
          renderImage(
            {
              list(
                src = plot_path,
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
          includeHTML(path2),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        ))
      } else if (input$conditionedPanels == "Assumption") {
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path <- "./www/assumptions.html"
        } else {
          path <- system.file("www/assumptions.html", package = "bs")
        }
        showModal(modalDialog(
          title = "Testing assumptions",
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (input$conditionedPanels == "Correlation") {
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path <- "./www/correlation.html"
        } else {
          path <- system.file("www/correlation.html", package = "bs")
        }
        showModal(modalDialog(
          title = "Correlation",
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (input$conditionedPanels == "Tests") {
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path <- "./www/tests.html"
        } else {
          path <- system.file("www/tests.html", package = "bs")
        }
        showModal(modalDialog(
          title = "Statistical tests",
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (input$conditionedPanels == "Dose Response analysis") {
        if (Sys.getenv("RUN_MODE") != "SERVER") {
          path <- "./www/doseresponse.html"
        } else {
          path <- system.file("www/doseresponse.html", package = "bs")
        }
        showModal(modalDialog(
          title = "Doseresponse analysis",
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    # docu formula editor
    observeEvent(input[["FO-formula_docu"]], {
      path <- ""
      if (Sys.getenv("RUN_MODE") != "SERVER") {
        path <- "./www/formula.html"
      } else {
        path <- system.file("www/formula.html", package = "bs")
      }
      showModal(modalDialog(
        title = "Defining the formula",
        includeHTML(path),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })
    # docu split by group
    observeEvent(input[["SG-split_docu"]], {
      path <- ""
      if (Sys.getenv("RUN_MODE") != "SERVER") {
        path <- "./www/SplitData.html"
      } else {
        path <- system.file("www/SplitData.html", package = "bs")
      }
      showModal(modalDialog(
        title = "Subsetting the dataset",
        includeHTML(path),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })

    download_file <- reactive({
      file <- download(session, "/home/shiny/results") # NOTE: from COMELN
      df <- NULL
      df <- readData(file)
      print_req(
        is.data.frame(df),
        "File can not be used. Upload into R failed!"
      )
      tryCatch(
        {
          unlink(file)
        },
        warning = function(warn) {
          print_warn(paste("A warning occurred: ", conditionMessage(warn)))
        },
        error = function(err) {
          print_err(paste("An error occurred: ", conditionMessage(err)))
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
    FormulaEditorServer("FO", dataSet, listResults)
    SplitByGroupServer("SG", dataSet, listResults)

    # Render results list
    output$Results <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
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
            message <- paste0(
              "Dose response analysis. (Outliers: ",
              paste0(temp@outlier_info, collapse = ";"),
              "). Too long to display",
              collapse = " "
            )
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
            listResults$history[[length(listResults$history) + 1]] <- list(
              type = "RemoveResult", entry_removed = name
            )
            listResults$all_data <- current_list
          },
          ignoreInit = TRUE
        )
      })
    })

    # Observe open formula editor
    output$open_formula_editor_main <- renderUI({
      if (input$conditionedPanels == "DataWrangling" ||
        input$conditionedPanels == "Visualisation") {
        return()
      }
      div(
        class = "boxed-output",
        actionButton("open_formula_editor",
          "Open formula editor",
          title = "Open the formula editor to create or modify a formula",
          disabled = is.null(dataSet$df) || !is.data.frame(dataSet$df)
        )
      )
    })
    observeEvent(input[["open_formula_editor"]], {
      showModal(modalDialog(
        title = "FormulaEditor",
        FormulaEditorUI("FO"),
        easyClose = TRUE,
        size = "l",
        footer = tagList(
          modalButton("Close")
        )
      ))
    })
    # display current formula
    observe({
      req(!is.null(dataSet$formula))
      output$formula <- renderText({
        deparse(dataSet$formula)
      })
    })
    output[["formulaUI"]] <- renderUI({
      if (input$conditionedPanels == "DataWrangling" ||
        input$conditionedPanels == "Visualisation") {
        return()
      } else {
        verbatimTextOutput("formula")
      }
    })

    # Render split by group
    output[["open_split_by_group"]] <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      div(
        class = "boxed-output",
        actionButton("open_split_by_group",
          "Open the split by group functionality",
          title = "Open the split by group helper window",
          disabled = is.null(dataSet$df) ||
            !is.data.frame(dataSet$df) ||
            !is.null(dataSet$backup_df)
        ),
        actionButton("remove_filter",
          "Remove the filter from the dataset",
          title = "remove the filter of the dataset",
          disabled = is.null(dataSet$backup_df) || !is.data.frame(dataSet$backup_df)
        )
      )
    })
    observeEvent(input[["open_split_by_group"]], {
      showModal(modalDialog(
        title = "SplitByGroup",
        SplitByGroupUI("SG"),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    })
    observe({
      output$applied_filter <- renderText(NULL)
      req(!is.null(dataSet$filter_col))
      req(!is.null(dataSet$filter_group))
      output$applied_filter <- renderText({
        paste(
          "The dataset is splitted by the variable(s): [",
          paste(dataSet$filter_col, collapse = ", "),
          "] group(s) are set to: [",
          paste(dataSet$filter_group, collapse = ", "),
          "]"
        )
      })
    })
    # Remove filter
    observeEvent(input[["remove_filter"]], {
      dataSet$df <- dataSet$backup_df
      listResults$history[[length(listResults$history) + 1]] <- list(
        type = "RemoveFilter",
        filter_col = paste(dataSet$filter_col, collapse = ", "),
        filter_group = paste(dataSet$filter_group, collapse = ", ")
      )
      dataSet$backup_df <- NULL
      dataSet$filter_col <- NULL
      dataSet$filter_group <- NULL
    })

    observeEvent(input$download, {
      if (!is_valid_filename(input$user_filename)) {
        runjs("document.getElementById('user_filename').focus();")
        print_noti(
          why_filename_invalid(input$user_filename)
        )
      }
      print_req(
        is_valid_filename(input$user_filename),
        "Defined filename is not valid"
      )
      print_req(length(listResults$all_data) > 0, "No results to save")
      l <- listResults$all_data
      str(listResults$history)
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        print_req(
          check_filename_for_server(input$user_filename),
          "Defined filename does not have xlsx as extension"
        )
        excelFile <- createExcelFile(l)
        upload(session, excelFile, new_name = input$user_filename)
      } else {
        print_req(
          check_filename_for_serverless(input$user_filename),
          "Defined filename does not have zip as extension"
        )
        jsString <- createJSString(l)
        session$sendCustomMessage(
          type = "downloadZip",
          list(
            numberOfResults = length(jsString),
            FileContent = jsString,
            Filename = input$user_filename,
            ResultNames = names(l)
          )
        )
      }
    })
  }

  return(list(ui = ui, server = server))
}
