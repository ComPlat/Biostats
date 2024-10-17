assSidebarUI <- function(id) {
  tabPanel(
    "Assumption",
    tags$hr(),
    textInput(NS(id, "dep"), "dependent Variable", value = "var1"),
    textInput(NS(id, "indep"), "independent Variable", value = "var2"),
    tags$hr(),
    tags$div(
      class = "header", checked = NA,
      tags$h4(
        style = "font-weight: bold;",
        "Test of normal distribution"
      )
    ),
    actionButton(NS(id, "shapiro"), "Shapiro test for individual groups"),
    tags$hr(),
    actionButton(NS(id, "shapiroResiduals"), "Shapiro test for residuals of linear model"),
    tags$hr(),
    tags$div(
      class = "header", checked = NA,
      tags$h4(
        style = "font-weight: bold;",
        "Test of variance homogenity"
      )
    ),
    actionButton(NS(id, "levene"), "Levene test"),
    selectInput(NS(id, "center"), "Data center of each group: mean or median",
      c(
        "Mean" = "mean",
        "Median" = "median"
      ),
      selectize = FALSE
    ),
    tags$hr(),
    tags$div(
      class = "header", checked = NA,
      tags$h4(style = "font-weight: bold;", "Visual tests")
    ),
    actionButton(NS(id, "DiagnosticPlot"), "diagnostic plots")
  )
}

assUI <- function(id) {
  fluidRow(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "download.js")
    ),
    h4(strong("Results of test:")),
    verbatimTextOutput(NS(id, "ass_error")),
    actionButton(NS(id, "ass_save"), "Add output to result-file"),
    actionButton(NS(id, "download_ass"), "Save and exit"),
    checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL),
    tableOutput(NS(id, "ass_result")),
    plotOutput(NS(id, "DiagnosticPlotRes"))
  )
}

assServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {
    runShapiro <- function() {
      output$ass_error <- renderText(NULL)
      req(input$indep)
      req(input$dep)
      indep <- input$indep
      dep <- input$dep
      df <- data$df
      req(is.data.frame(df))
      check <- TRUE
      res <- NULL
      temp <- NULL
      err <- NULL
      if (isTRUE(check)) {
        res <- list()
        e <- try({
          formula <- as.formula(paste(dep, "~", indep))
          stopifnot(get_ast(formula) != "Error")
          dat <- splitData(df, formula)
          for (i in unique(dat[, 2])) {
            tempDat <- dat[dat[, 2] == i, ]
            temp <- broom::tidy(shapiro.test(tempDat[, 1]))
            if (!is.null(temp)) {
              temp$variable <- i
              res[[length(res) + 1]] <- temp
            }
          }
          res <- do.call(rbind, res)
        })
        if (!inherits(e, "try-error")) {
          listResults$curr_data <- res
          listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted shapiro test")
          output$curr_result <- renderTable(res, digits = 6)
          output$curr_error <- renderText(err)
        } else {
          err <- conditionMessage(attr(e, "condition"))
          output$ass_error <- renderText(err)
        }
      }
    }
    observeEvent(input$shapiro, {
      runShapiro()
    })

    runShapiroResiduals <- function() {
      output$ass_error <- renderText(NULL)
      req(input$indep)
      indep <- input$indep
      req(input$dep)
      dep <- input$dep
      df <- data$df
      req(is.data.frame(df))
      formula <- NULL
      err <- NULL
      res <- NULL
      e <- try({
        formula <- as.formula(paste(dep, "~", indep))
        stopifnot(get_ast(formula) != "Error")
        fit <- lm(formula, data = df)
        r <- resid(fit)
        res <- broom::tidy(shapiro.test(r))
      })
      if (!inherits(e, "try-error")) {
        listResults$curr_data <- res
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted shapiro test")
        output$curr_result <- renderTable(res, digits = 6)
        output$curr_error <- renderText(err)
      } else {
        err <- conditionMessage(attr(e, "condition"))
        output$ass_error <- renderText(err)
      }
    }
    observeEvent(input$shapiroResiduals, {
      runShapiroResiduals()
    })

    runLevene <- function() {
      output$ass_error <- renderText(NULL)
      req(input$indep)
      indep <- input$indep
      req(input$dep)
      dep <- input$dep
      df <- data$df
      req(is.data.frame(df))
      formula <- NULL
      err <- NULL
      fit <- NULL
      e <- try({
        formula <- as.formula(paste(dep, "~", indep))
        stopifnot(get_ast(formula) != "Error")
        fit <- broom::tidy(car::leveneTest(formula, data = df, center = input$center))
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$ass_error <- renderText(err)
      } else {
        listResults$curr_data <- fit
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "variance homogenity (levene)")
        output$curr_result <- renderTable(fit, digits = 6)
        output$curr_error <- renderText(err)
      }
    }
    observeEvent(input$levene, {
      runLevene()
    })

    output$ass_result <- renderTable(
      {
        if (!inherits(listResults$curr_data, "diagnosticPlot")) {
          return(listResults$curr_data)
        }
        return(NULL)
      },
      digits = 6
    )

    runDiagnosticPlot <- function() {
      output$ass_error <- renderText(NULL)
      req(input$indep)
      indep <- input$indep
      req(input$dep)
      dep <- input$dep
      df <- data$df
      req(is.data.frame(df))
      formula <- NULL
      err <- NULL
      f <- NULL
      e <- try({
        formula <- as.formula(paste(dep, "~", indep))
        stopifnot(get_ast(formula) != "Error")
        f <- diagnosticPlot(df, formula)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$ass_error <- renderText(err)
      } else {
        listResults$curr_data <- new("diagnosticPlot", p = f)
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "diagnostic plots")
        output$DiagnosticPlotRes <- renderImage(
          {
            list(
              src = f,
              contentType = "image/png"
            )
          },
          deleteFile = FALSE
        )
        output$curr_error <- renderText(err)
      }
    }
    observeEvent(input$DiagnosticPlot, {
      runDiagnosticPlot()
    })

    observeEvent(input$ass_save, {
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

    observeEvent(input$download_ass, {
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

  return(listResults)
}
