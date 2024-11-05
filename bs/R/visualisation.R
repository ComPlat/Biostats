visSidebarUI <- function(id) {
  tabPanel(
    "Visualisation",
    div(
      class = "boxed-output",
      uiOutput(NS(id, "open_split_by_group")),
      uiOutput(NS(id, "data_splitted")),
      verbatimTextOutput(NS(id, "applied_filter"))
    ),

    uiOutput(NS(id, "yVar")),
    uiOutput(NS(id, "xVar")),
    radioButtons(NS(id, "xType"), "Type of x",
      choices = c(
        factor = "factor",
        numeric = "numeric"
      ),
      selected = "factor"
    ),
    textInput(NS(id, "xaxisText"), "X axis label", value = "x label"),
    textInput(NS(id, "yaxisText"), "Y axis label", value = "y label"),
    conditionalPanel(
      condition = "input.VisConditionedPanels == 'Boxplot'",
      uiOutput(NS(id, "fill")),
      textInput(NS(id, "legendTitleFill"), "Legend title for fill", value = "Title fill"),
      selectInput(NS(id, "themeFill"), "Choose a 'fill' theme",
        c(
          "BuGn" = "BuGn",
          "PuRd" = "PuRd",
          "YlOrBr" = "YlOrBr",
          "Greens" = "Greens",
          "GnBu" = "GnBu",
          "Reds" = "Reds",
          "Oranges" = "Oranges",
          "Greys" = "Greys"
        ),
        selectize = FALSE
      )
    ),
    uiOutput(NS(id, "col")),
    textInput(NS(id, "legendTitleCol"), "Legend title for colour", value = "Title colour"),
    selectInput(NS(id, "theme"), "Choose a 'colour' theme",
      c(
        "Accent" = "Accent",
        "Dark2" = "Dark2",
        "Paired" = "Paired",
        "Pastel1" = "Pastel1",
        "Pastel2" = "Pastel2",
        "Set1" = "Set1",
        "Set2" = "Set2",
        "Set3" = "Set3"
      ),
      selectize = FALSE
    ),
    uiOutput(NS(id, "facetBy"))
  )
}

visUI <- function(id) {
  fluidRow(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "download.js"),
    ),
    br(),
    tabsetPanel(
      tabPanel(
        "Boxplot",
        br(),
        actionButton(NS(id, "CreatePlotBox"), "Create plot")
      ),
      tabPanel(
        "Scatterplot",
        br(),
        actionButton(NS(id, "CreatePlotScatter"), "Create plot")
      ),
      tabPanel(
        "Lineplot",
        br(),
        actionButton(NS(id, "CreatePlotLine"), "Create plot")
      ),
      id = "VisConditionedPanels"
    ),
    plotOutput(NS(id, "plotResult")),
    actionButton(NS(id, "plotSave"), "Add output to result-file"),
    checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL),
    fluidRow(
      column(
        4,
        numericInput(NS(id, "widthPlot"), "Width of plot [cm]", value = 10)
      ),
      column(
        4,
        numericInput(NS(id, "heightPlot"), "Height of plot [cm]", value = 10)
      ),
      column(
        4,
        numericInput(NS(id, "resPlot"), "Resolution of plot", value = 300)
      ),
    ),
    fluidRow(
      column(
        12,
        actionButton(NS(id, "downloadViss"), "Save results")
      )
    )
  )
}

visServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {

    # Render x and y selectInput
    output[["yVar"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- names(data$df)
      tooltip <- "Select the value of the Y variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-yVar"),
          label = "Y Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    output[["xVar"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- names(data$df)
      tooltip <- "Select the value of the X variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-xVar"),
          label = "X Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    output[["col"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- c("", names(data$df))
      tooltip <- "Select the value of the colour variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-col"),
          label = "Colour Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    output[["fill"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- c("", names(data$df))
      tooltip <- "Select the value of the fill variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-fill"),
          label = "Fill Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    output[["facetBy"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- c("", names(data$df))
      tooltip <- "Split plot in panels based on which variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-facetBy"),
          label = "Split Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    # Render split by group
    output$open_split_by_group <- renderUI({
      actionButton(NS(id, "open_split_by_group"),
        "Open the split by group functionality",
        title = "Open the split by group helper window",
        disabled = is.null(data$df) || !is.data.frame(data$df)
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

    # check if data is splitted
    output$data_splitted <- renderUI({
      actionButton(NS(id, "remove_filter"),
        "Remove the filter from the dataset",
        title = "remove the filter of the dataset",
        disabled = is.null(data$backup_df) || !is.data.frame(data$backup_df)
      )
    })

    observe({
      output$applied_filter <- renderText(NULL)
      req(!is.null(data$filter_col))
      req(!is.null(data$filter_group))
      output$applied_filter <- renderText({
        paste0(
          "The dataset is splitted by the variable ",
          data$filter_col,
          " and the group is ",
          data$filter_group)
      })
    })

    # Remove filter
    observeEvent(input[["remove_filter"]], {
      data$df <- data$backup_df
      data$backup_df <- NULL
      data$filter_col <- NULL
      data$filter_group <- NULL
    })

    # Plot stuff
    plotFct <- function(method) {
      req(is.data.frame(data$df))
      df <- data$df
      req(input$yVar)
      req(input$xVar)
      x <- input$xVar
      y <- input$yVar
      colNames <- names(df)
      checkX <- x %in% colNames
      checkY <- y %in% colNames
      if (!checkX) showNotification("X variable not found", duration = 0)
      if (!checkY) showNotification("Y variable not found", duration = 0)
      req(checkX)
      req(checkY)
      width <- input$widthPlot
      height <- input$heightPlot
      resolution <- input$resPlot
      if (width <= 0) {
        showNotification(paste("width has to be a positive number is changed to 10 cm"), duration = 0)
        width <- 10
      }
      if (height <= 0) {
        showNotification(paste("height has to be a positive number is changed to 10 cm"), duration = 0)
        height <- 10
      }
      if (width > 100) {
        showNotification(paste("width exceeds max value of 100 cm. Is set to 100 cm."), duration = 0)
        width <- 100
      }
      if (height > 100) {
        showNotification(paste("height exceeds max value of 100 cm. Is set to 100 cm."), duration = 0)
        height <- 100
      }
      col <- input$col
      fill <- input$fill
      if (!(fill %in% names(df)) && (fill != "")) showNotification("fill variable not found", duration = 0)
      if (!(col %in% names(df)) && (col != "")) showNotification("colour variable not found", duration = 0)
      req((fill %in% names(df)) || (fill == ""))
      req((col %in% names(df)) || (col == ""))
      fillTitle <- input$legendTitleFill
      colTitle <- input$legendTitleCol
      xlabel <- input$xaxisText
      ylabel <- input$yaxisText
      xtype <- input$xType
      theme <- input$theme
      themeFill <- input$themeFill
      facetMode <- "none"
      facet <- ""
      if (input$facetBy != "") {
        facet <- input$facetBy
        facetMode <- "facet_wrap"
      }
      fitMethod <- "none"
      xd <- NULL
      if (xtype == "numeric") {
        xd <- as.numeric(df[, x])
      } else {
        xd <- as.factor(df[, x])
      }
      yd <- as.numeric(df[, y])
      if (fitMethod != "none" && !is.null(fitMethod) && xtype != "numeric") {
        showNotification("Fit method will be ignored as X variable is not numerical", duration = 0)
        fitMethod <- "none"
      }

      p <- tryCatch(
        {
          if (method == "box") {
            p <- BoxplotFct(
              df, x, y, xlabel, ylabel,
              fill, fillTitle, themeFill,
              col, colTitle, theme,
              facetMode, facet
            )
          } else if (method == "dot") {
            k <- NULL
            if (fitMethod == "gam") {
              req(input$k)
              k <- input$k
              if (k <= 0) {
                showNotification("k has to be at least 1 and is set to this value")
                k <- 1
              }
            }
            p <- DotplotFct(
              df, x, y, xlabel, ylabel,
              fitMethod,
              col, colTitle, theme,
              facetMode, facet, k
            )
          } else if (method == "line") {
            p <- LineplotFct(
              df, x, y, xlabel, ylabel,
              col, colTitle, theme,
              facetMode, facet
            )
          }
        },
        warning = function(warn) {
          showNotification(paste("A warning occurred: ", conditionMessage(warn)), duration = 0)
        },
        error = function(err) {
          showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
        }
      )
      output$plotResult <- renderPlot(p)
      listResults$curr_data <- new("plot", p = p, width = width, height = height, resolution = resolution)
      listResults$curr_name <- paste(
        "Plot Nr",
        length(listResults$all_names) + 1, paste("Type: ", method)
      )
    }

    observeEvent(input$CreatePlotBox, {
      req(is.data.frame(data$df))
      plotFct("box")
    })
    output$plotResult <- renderPlot({
      renderPlot(listResults$curr_data)
    })

    observeEvent(input$CreatePlotScatter, {
      req(is.data.frame(data$df))
      plotFct("dot")
    })
    output$plotResult <- renderPlot({
      renderPlot(listResults$curr_data)
    })

    observeEvent(input$CreatePlotLine, {
      req(is.data.frame(data$df))
      plotFct("line")
    })
    output$plotResult <- renderPlot({
      renderPlot(listResults$curr_data)
    })

    observeEvent(input$plotSave, {
      if (is.null(listResults$curr_name)) {
        return(NULL)
      }
      if (!(listResults$curr_name %in% unlist(listResults$all_names))) {
        listResults$all_data[[length(listResults$all_data) + 1]] <- listResults$curr_data
        listResults$all_names[[length(listResults$all_names) + 1]] <- listResults$curr_name
      }
      updateCheckboxGroupInput(session, "TableSaved",
        choices = listResults$all_names
      )
    })

    observeEvent(input$downloadViss, {
      lr <- unlist(listResults$all_names)
      indices <- sapply(input$TableSaved, function(x) {
        which(x == lr)
      })
      req(length(indices) >= 1)
      l <- listResults$all_data[indices]
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        excelFile <- createExcelFile(l)
        upload(session, excelFile, new_name = "Results.xlsx") # TODO: add possibility for desired file name
      } else {
        jsString <- createJSString(l)
        session$sendCustomMessage(
          type = "downloadZip",
          list(
            numberOfResults = length(jsString),
            FileContent = jsString
          )
        )
      }
    })
  })
}
