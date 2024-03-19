visSidebarUI <- function(id) {
  tabPanel(
    "Visualisation",
    textInput(NS(id , "yVar"), "Y variable", value = "y"),
    textInput(NS(id, "xVar"), "X variable", value = "x"),
    radioButtons(NS(id, "xType"), "Type of x",
                 choices = c(
                   factor = "factor",
                   numeric = "numeric"
                 ),
                 selected = "factor"
    ),
    textInput(NS(id, "xaxisText"), "X axis label", value = "x label"),
    textInput(NS(id, "yaxisText"), "Y axis label", value = "y label"),
    selectInput(NS(id, "fitMethod"), "Choose a fitting method",
                c(
                  "none" = "none",
                  "lm" = "lm",
                  "glm" = "glm",
                  "gam" = "gam",
                  "loess" = "loess"
                ),
                selectize = FALSE
    ),
    textInput(NS(id, "fill"), "Fill variable"),
    textInput(NS(id, "legendTitleFill"), "Legend title for fill", value = "Title fill"),
    textInput(NS(id, "col"), "Colour variable"),
    textInput(NS(id, "legendTitleCol"), "Legend title for colour", value = "Title colour"),
    selectInput(NS(id, "theme"), "Choose a 'colour' theme",
                c(
                  "BuPu" = "BuPu",
                  "RdYIBu" = "RdYIBu",
                  "Paired" = "Paired",
                  "PuOr" = "PuOr",
                  "Spectral" = "Spectral",
                  "Pastel1" = "Pastel1",
                  "hue" = "hue",
                  "grey" = "grey"
                ),
                selectize = FALSE
    ),
    selectInput(NS(id, "themeFill"), "Choose a 'fill' theme",
                c(
                  "BuPu" = "BuPu",
                  "RdYIBu" = "RdYIBu",
                  "Paired" = "Paired",
                  "PuOr" = "PuOr",
                  "Spectral" = "Spectral",
                  "Pastel1" = "Pastel1",
                  "hue" = "hue",
                  "grey" = "grey"
                ),
                selectize = FALSE
    ),
    radioButtons(NS(id, "facetMode"),
                 "Choose Facet Mode:",
                 choices = c("none", "facet_wrap", "facet_grid")
    ),
    textInput(NS(id, "facetBy"), "split plot by")
    )
}

visUI <- function(id) {
  fluidRow(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "download.js")
    ),
    br(),
    tabsetPanel(
      tabPanel("Boxplot",
          br(),
          actionButton(NS(id, "CreatePlotBox"), "Create plot")
      ),
      tabPanel("Scatterplot",
          br(),
          actionButton(NS(id, "CreatePlotScatter"), "Create plot") 
      ),
      tabPanel("Lineplot",
          br(),
          actionButton(NS(id, "CreatePlotLine"), "Create plot")
      ),
      id = "VisConditionedPanels"   
    ),
    plotOutput(NS(id, "plotResult")),
    actionButton(NS(id, "plotSave"), "Add output to result-file"),
    checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL),
    fluidRow(
      column(4,
        numericInput(NS(id, "widthPlot"), "Width of plot [cm]", value = 10)
      ),
      column(4,
        numericInput(NS(id, "heightPlot"), "Height of plot [cm]", value = 10)
      ),
      column(4,
        numericInput(NS(id, "resPlot"), "Resolution of plot", value = 300)
      ),
    ),
    fluidRow(
      column(12,
        actionButton(NS(id, "downloadViss"), "Save results")
      )
    )
  )
}

visServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {  
    
    plotFct <- function(method) {
      req(is.data.frame(data$df))
      df <- data$df
      req(input$yVar)
      req(input$xVar)
      x <- input$xVar; y <- input$yVar
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
      if ( !(fill %in% names(df)) && (fill != "") ) showNotification("fill variable not found", duration = 0)
      if ( !(col %in% names(df)) && (fill != "") ) showNotification("colour variable not found", duration = 0)
      req( (fill %in% names(df)) || (fill == "") )
      req( (col %in% names(df)) || (col == "") )
      fillTitle <- input$legendTitleFill
      colTitle <- input$legendTitleCol
      xlabel <- input$xaxisText
      ylabel <- input$yaxisText
      xtype <- input$xType
      theme <- input$theme
      themeFill <- input$themeFill
      facetMode <- input$facetMode
      facet <- input$facetBy
      fitMethod <- input$fitMethod
      
      xd <- NULL
      if (xtype == "numeric") {
        xd <- as.numeric(df[,x])
      } else {
        xd <- as.factor(df[,x])
      }
      yd <- as.numeric(df[,y])
      if (fitMethod != "none" && !is.null(fitMethod) && xtype != "numeric") {
        showNotification("Fit method will be ignored as X variable is not numerical", duration = 0)
      }
      
      pfct <- function() {
        if ( (fill == "") && (col == "")) {
          p <- ggplot(data = df, aes(x = xd, y = yd, group = xd)) +
            ylab(ylabel) +
            xlab(xlabel)  
        } else if ((fill != "") && (col != "") ) {
          p <- ggplot(data = df,
                      aes(x = xd, y = yd, group = xd,
                          fill = df[,fill], color = df[,col]) ) +
            ylab(ylabel) +
            xlab(xlabel) +
            guides(fill = guide_legend(title = fill_title), col = guide_legend(title = col_title))       
        } else if ( (fill != "") && (col == "") ) {
          p <- ggplot(data = df,
                      aes(x = xd, y = yd, group = xd, 
                          fill = df[,fill]) ) +
            ylab(ylabel) +
            xlab(xlabel) +
            guides(fill = guide_legend(title = fill_title) )
        } else if ( (fill == "") && (col != "") ) {
          p <- ggplot(data = df,
                      aes(x = xd, y = yd, group = xd,
                          color = df[,col]) ) +
            ylab(ylabel) +
            xlab(xlabel) +
            guides(col = guide_legend(title = col_title) )
        }
        if (method == "box") {
          p <- p +
            scale_color_brewer(palette = theme) +
            scale_fill_brewer(palette = themeFill)
          p <- p + geom_boxplot()
          if (fitMethod != "" && !is.null(fitMethod) && fitMethod != "none") {
            p <- annotatePlot(p, fitMethod)
          } 
        } else if (method == "dot") {
          p <- p +
            scale_color_brewer(palette = theme) 
          p <- p + geom_point() +   geom_smooth(method = fitMethod) 
          if (fitMethod != "" && !is.null(fitMethod) && fitMethod != "none") {
            p <- annotatePlot(p, fitMethod)
          } 
        } else if (method == "line") {
          p <- p +
            scale_color_brewer(palette = theme) 
          p <- p + geom_line()
          if (fitMethod != "" && !is.null(fitMethod) && fitMethod != "none") {
            p <- annotatePlot(p, fitMethod)
          } 
        }  
        if (facetMode == "facet_wrap") {
          p <- p + facet_wrap(~ df[,facet], scales = "free")
        } else if (facetMode == "facet_grid") {
          p <- p + facet_grid(. ~  df[,facet], scales = "free")
        }
        return(p)
      }
      
      p <- tryCatch({
        p <- pfct()
      }, 
      warning = function(warn) {
        showNotification(paste("A warning occurred: ", conditionMessage(warn)), duration = 0)
      }, 
      error = function(err) {
        showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
      })
      output$plotResult <- renderPlot(p)
      listResults$curr_data <- p
      listResults$curr_name <- paste("Plot Nr",
                                     length(listResults$all_names) + 1,  paste("Type: ", method))
      listResults$all_data[[length(listResults$all_data) + 1]] <- p
      listResults$all_names[[length(listResults$all_names) + 1]] <- listResults$curr_name
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
      updateCheckboxGroupInput(session, "TableSaved",
                               choices = listResults$all_names)
    })
    
    observeEvent(input$downloadViss, {
      indices <- which(input$TableSaved == listResults$all_names)
      req(length(indices) >= 1)
      l <- listResults$all_data[indices]
      jsString <- character(length(l))
      for (i in seq_along(l)) {
        if (inherits(l[[i]], "ggplot")) {
          fn <- tempfile(fileext = '.png')
          ggsave(plot = l[[i]], filename = fn) # issue: add resoultion, width and height
          jsString[i] <- paste0("data:image/png;base64,", base64enc::base64encode(fn))
          unlink(fn)
        } else if (inherits(l[[i]], "data.frame")) {
          jsString[i] <- DF2String(l[[i]])
        } else if (is.character(l[[i]])) {
          jsString[i] <- l[[i]]
        }
      }
      session$sendCustomMessage(type = "downloadZip",
                                list(numberOfResults = length(jsString),
                                     FileContent = jsString))
    })
    
    
  })
}
