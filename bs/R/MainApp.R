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
        includeHTML("www/correlation.html"),
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
        HTML(
          '
          <p>The <strong>Visualisation Tab</strong>
          in Biostats allows users to explore and visualize data through several plot types,
          including <em>Boxplots</em>, <em>Scatterplots</em>, and <em>Lineplots</em>.
          Below is an outline of each feature, including controls and customization options.
          </p>
          <div class="boxed-output">
          <p><strong>Data Grouping and Splitting</strong></p>
          <p>Use these options to separate data into groups or apply filters dynamically:</p>
          <ul>
          <li><strong>Split by Group</strong>: Enables user to subset data using one or more column(s).</li>
          </ul>
          </div>
          '
        ),
        br(),
        renderImage({
          list(src = "www/DocuPlot.jpg",
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
        HTML(
          '
          <div class="boxed-output">
          <p><strong>Variable Selection</strong></p>
          <p>Select variables for both the x-axis and y-axis:</p>
          <ul>
          <li><strong>Y Variable</strong>: Set the variable for the y-axis.</li>
          <li><strong>X Variable</strong>: Set the variable for the x-axis, with options to treat it as a factor or numeric. (see plot a)</li>
          </ul>
          </div>

          <div class="boxed-output">
          <p><strong>Conditional Options</strong></p>
          <p>Customize your plot with the following settings:</p>
          <ul>
          <li>Choose columns of the dataset which are used to further distinguish the data. </li>
          <li>In case of boxplots groups can be defined for the colour of the box border and the fill of the box (See plot b and c)</li>
          <li>The other plot types only support the colour attribute (see plot c).</li>
          <li><strong>Legend Titles</strong>: Set titles for fill and color legends to improve interpretability.</li>
          </ul>
          </div>

          <div class="boxed-output">
          <p><strong>Axis and Facet Controls</strong></p>
          <p>Adjust axis labels, ranges, and facet options:</p>
          <ul>
          <li><strong>Axis Labels</strong>: Set custom labels for x and y axes.</li>
          <li><strong>Axis Ranges</strong>: Define minimum and maximum values for zooming in on data.</li>
          <li><strong>Split plot by a column</strong> Specify a variable to create several subplots (see plot d)</li>
          </ul>
          </div>

          <div class="boxed-output">
          <p><strong>Plot Type Selection and Plot Creation</strong><p>
          <p>Choose between Boxplot, Scatterplot, and Lineplot, and click "Create Plot" to generate:</p>
          <p>Use the interactive adjustments for dimensions and resolution before saving or exporting plots.</p>
          </div>

          <div class="boxed-output">
          <p><strong>Saving and Exporting Options</strong></p>
          <p>Once satisfied with the plot, use these controls to save and export:</p>
          <ul>
          <li><strong>Add to Result File</strong>: Adds the current result to the <em>result file</em>.
          This is a temporary file holding all results.</li>
          <li><strong>Final selection</strong>: tick the boxes from the <em>result file</em> to add them to the result file. </li>
          <li><strong>Download Options</strong>: Export plots with custom width, height, and resolution.</li>
          <li><strong>Save results</strong>: by clicking this button the file is either sent to your ELN (please refresh the page) or saved locally.</li>
          </ul>
          </div>
          '
        ),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })
    # docu formula editor
    observeEvent(input[["FO-formula_docu"]], {
      showModal(modalDialog(
        title = "Defining the formula",
        includeHTML("www/formula.html"),
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
        includeHTML("www/SplitData.html"),
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
