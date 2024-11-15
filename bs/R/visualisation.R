visSidebarUI <- function(id) {
  tabPanel(
    "Visualisation",
    div(
      class = "boxed-output",
      uiOutput(NS(id, "open_split_by_group")),
      uiOutput(NS(id, "data_splitted")),
      verbatimTextOutput(NS(id, "applied_filter"))
    ),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "yVarUI")),
      uiOutput(NS(id, "xVarUI")),
      textInput(NS(id, "xaxisText"), "X axis label", value = "x label"),
      textInput(NS(id, "yaxisText"), "Y axis label", value = "y label")
    ),
    div(
      class = "boxed-output",
      conditionalPanel(
        condition = "input.VisConditionedPanels == 'Boxplot'",
        uiOutput(NS(id, "fillUI")),
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
      )
    ),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "colUI")),
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
      )
    ),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "facetByUI")),
      uiOutput(NS(id, "facetScalesUI"))
    ),
    div(
      class = "boxed-output",
      radioButtons(NS(id, "xType"), "Type of x",
        choices = c(
          factor = "factor",
          numeric = "numeric"
        ),
        selected = "factor"
      ),
      uiOutput(NS(id, "XRangeUI")),
      uiOutput(NS(id, "YRangeUI"))
    )
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
        actionButton(NS(id, "downloadViss"), "Save results"),
        textInput(NS(id, "user_filename"), "Set filename", value = "")
      )
    ),
    plotOutput(
      NS(id, "plotResult")
    )
  )
}

visServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {

    # Render axis limits
    output[["XRangeUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      req(input$xVar)
      x <- input$xVar
      df <- data$df
      if (is.numeric(df[, x])) {
        x_min <- min(df[[x]], na.rm = TRUE)
        padded_min <- x_min * 0.5 # Needed for boxplots
        x_max <- max(df[[x]], na.rm = TRUE)
        padded_max <- x_max * 1.25
        return(
          sliderInput(
            "VIS-XRange",
            "Select range for x axis:",
            min = padded_min,
            max = padded_max,
            value = c(padded_min, padded_max)
          )
        )
      } else {
        choices <- unique(df[[x]])
        return(
          shinyWidgets::sliderTextInput( # TODO: add everywhere shinyWidgets
            "VIS-XRange",
            "Select range for x axis:",
            selected = c(choices[1], choices[length(choices)]),
            choices = choices
          )
        )
      }
    })

    output[["YRangeUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      req(input$yVar)
      y <- input$yVar
      df <- data$df
      if (is.numeric(df[, y])) {
        y_min <- min(df[[y]], na.rm = TRUE)
        padded_min <- y_min * 0.95
        y_max <- max(df[[y]], na.rm = TRUE)
        padded_max <- y_max * 1.05
        return(
          sliderInput(
            "VIS-YRange",
            "Select range for y axis:",
            min = padded_min,
            max = padded_max,
            value = c(padded_min, padded_max)
          )
        )
      } else {
        choices <- unique(df[[y]])
        return(
          shinyWidgets::sliderTextInput( # TODO: add everywhere shinyWidgets
            "VIS-YRange",
            "Select range for x axis:",
            selected = c(choices[1], choices[length(choices)]),
            choices = choices
          )
        )
      }
    })

    # Render x and y selectInput
    output[["yVarUI"]] <- renderUI({
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

    output[["xVarUI"]] <- renderUI({
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

    output[["colUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- c("", names(data$df))
      tooltip <- "Select a variable  for the colour variable. By chosing this groups are formed based on the unique entries in this column. Thereby, each entry gets its own colour to distinguish the groups. Dependent on the plot type either the lines, dots or the frame of the boxes are labelled"
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
          selected = character(0)
        )
      )
    })

    output[["fillUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- c("", names(data$df))
      tooltip <- "Select a variable  for the fill variable. By chosing this groups are formed based on the unique entries in this column. Thereby, each entry gets its own colour to distinguish the groups."
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

    output[["facetByUI"]] <- renderUI({
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

    # TODO: Why is this defined as renderUI?
    output[["facetScalesUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- c("", names(data$df))
      tooltip <- "Do you want to scale the Y axis"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        radioButtons(
          "VIS-facetScales",
          "Scaling of y axis",
          choices = c(
            free = "free",
            fixed = "fixed"
          ),
          selected = "free"
        )
      )
    })

    # Render split by group
    output[["open_split_by_group"]] <- renderUI({
      actionButton(NS(id, "open_split_by_group"),
        "Open the split by group functionality",
        title = "Open the split by group helper window",
        disabled = is.null(data$df) || !is.data.frame(data$df) || !is.null(data$backup_df)
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
        paste(
          "The dataset is splitted by the variable(s): [",
          paste(data$filter_col, collapse = ", "),
          "] group(s) are set to: [",
          paste(data$filter_group, collapse = ", "),
          "]"
        )
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
      print_noti(x %in% colNames, "X variable not found")
      print_noti(y %in% colNames, "Y variable not found")
      width <- input$widthPlot
      height <- input$heightPlot
      resolution <- input$resPlot
      print_noti(width > 0, "width has to be a positive number; It is changed to 10 cm")
      if (width <= 0) width <- 10
      print_noti(height > 0, "height has to be a positive number; It is changed to 10 cm")
      if (height <= 0) height <- 10
      print_noti(width < 100, "width exceeds max value of 100; It is changed to 100 cm")
      if (width > 100) width <- 100
      print_noti(height < 100, "height exceeds max value of 100; It is changed to 100 cm")
      if (height > 100) height <- 100
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
      facetScales <- input$facetScales
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
              facetMode, facet, facetScales,
              input$XRange[1], input$XRange[2], input$YRange[1], input$YRange[2]
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
              facetMode, facet, facetScales, k,
              input$XRange[1], input$XRange[2], input$YRange[1], input$YRange[2]
            )
          } else if (method == "line") {
            p <- LineplotFct(
              df, x, y, xlabel, ylabel,
              col, colTitle, theme,
              facetMode, facet, facetScales,
              input$XRange[1], input$XRange[2], input$YRange[1], input$YRange[2]
            )
          }
          ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
          p
        },
        warning = function(warn) {
          showNotification(warn$message)
          return(p)
        },
        error = function(err) {
          showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
        }
      )
      exportTestValues(
        plot = p
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
      print_noti(is_valid_filename(input$user_filename), "Defined filename is not valid")
      lr <- unlist(listResults$all_names)
      indices <- sapply(input$TableSaved, function(x) {
        which(x == lr)
      })
      req(length(indices) >= 1)
      l <- listResults$all_data[indices]
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        print_noti(check_filename_for_server(input$user_filename), "Defined filename does not have xlsx as extension")
        excelFile <- createExcelFile(l)
        upload(session, excelFile, new_name = input$user_filename)
      } else {
        print_noti(check_filename_for_serverless(input$user_filename), "Defined filename does not have zip as extension")
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
  })
}
