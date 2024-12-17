visSidebarUI <- function(id) {
  tabPanel(
    "Visualisation",
    br(),
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
          shinyWidgets::sliderTextInput(
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
          shinyWidgets::sliderTextInput(
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

    # Plot stuff
    plotFct <- function(method) {
      print_req(is.data.frame(data$df), "The dataset is missing")
      df <- data$df
      x <- input$xVar
      y <- input$yVar
      colNames <- names(df)
      width <- input$widthPlot
      height <- input$heightPlot
      resolution <- input$resPlot
      print_req(width > 0, "width has to be a positive number; It is changed to 10 cm")
      if (width <= 0) width <- 10
      print_req(height > 0, "height has to be a positive number; It is changed to 10 cm")
      if (height <= 0) height <- 10
      print_req(width < 100, "width exceeds max value of 100; It is changed to 100 cm")
      if (width > 100) width <- 100
      print_req(height < 100, "height exceeds max value of 100; It is changed to 100 cm")
      if (height > 100) height <- 100
      col <- input$col
      fill <- input$fill
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
        print_warn("Fit method will be ignored as X variable is not numerical")
        fitMethod <- "none"
      }
      p <- tryCatch(
        {
          withCallingHandlers(
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
                    print_warn("k has to be at least 1 and is set to this value")
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
            },
            warning = function(warn) {
              print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
          check_rls(listResults$all_data, p)
          ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
          p
        },
        warning = function(warn) {
          print_warn(warn$message)
          return(p)
        },
        error = function(err) {
          print_err(paste("An error occurred: ", conditionMessage(err)))
        }
      )
      exportTestValues(
        plot = p
      )
      listResults$counter <- listResults$counter + 1
      new_result_name <- paste0("PlotNr", listResults$counter)
      listResults$all_data[[new_result_name]] <- new("plot", p = p, width = width, height = height, resolution = resolution)
    }

    observeEvent(input$CreatePlotBox, {
      print_req(is.data.frame(data$df), "The dataset is missing")
      plotFct("box")
    })

    observeEvent(input$CreatePlotScatter, {
      print_req(is.data.frame(data$df), "The dataset is missing")
      plotFct("dot")
    })

    observeEvent(input$CreatePlotLine, {
      print_req(is.data.frame(data$df), "The dataset is missing")
      plotFct("line")
    })
  })
}
