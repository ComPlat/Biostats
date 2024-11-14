assSidebarUI <- function(id) {
  tabPanel(
    "Assumption",
    tags$hr(),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "open_formula_editor_corr")),
      verbatimTextOutput(NS(id, "formula"))
    ),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "open_split_by_group")),
      uiOutput(NS(id, "data_splitted")),
      verbatimTextOutput(NS(id, "applied_filter"))
    ),
    tags$hr(),
    tags$div(
      class = "header", checked = NA,
      tags$h4(
        style = "font-weight: bold;",
        "Test of normal distribution"
      )
    ),
    actionButton(NS(id, "shapiro"),
      "Shapiro test for individual groups",
      title = 
      "Use this test if you have a formula like 'response ~ pred1 * pred2' (two-way ANOVA) to check normality of residuals within each group."),
    tags$hr(),
    actionButton(NS(id, "shapiroResiduals"), "Shapiro test for residuals of linear model",
      title = 
      "Use this test if you have a formula like 'response ~ predictor1' to check normality of the residuals of the linear model."
    ),
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
    textInput(NS(id, "user_filename"), "Set filename", value = ""),
    checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL),
    tableOutput(NS(id, "ass_result")),
    plotOutput(NS(id, "DiagnosticPlotRes"), width = "100%", height = "1000px")
  )
}

assServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {
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

    output$open_formula_editor_corr <- renderUI({ # TODO: change to unique identifier probably via [["open_formula_editor"]]
      actionButton(NS(id, "open_formula_editor"),
        "Open formula editor",
        title = "Open the formula editor to create or modify a formula",
        disabled = is.null(data$df) || !is.data.frame(data$df)
      )
    })

    observeEvent(input[["open_formula_editor"]], {
      showModal(modalDialog(
        title = "FormulaEditor",
        FormulaEditorUI("FO"),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    })

    # display current formula
    observe({
      req(!is.null(data$formula))
      output$formula <- renderText({deparse(data$formula)})
    })

    runShapiro <- function() {
      output$ass_error <- renderText(NULL)
      df <- data$df
      req(is.data.frame(df))
      req(!is.null(data$formula))
      formula <- data$formula
      check <- TRUE
      res <- NULL
      temp <- NULL
      err <- NULL
      if (isTRUE(check)) {
        res <- list()
        e <- try({
          dat <- splitData(df, formula)
          for (i in unique(dat[, 2])) {
            tempDat <- dat[dat[, 2] == i, ]
            temp <- broom::tidy(shapiro.test(tempDat[, 1]))
            if (!is.null(temp)) {
              temp$variable <- i
              temp$`Normal distributed` <- temp$p.value > 0.05
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
      df <- data$df
      req(is.data.frame(df))
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      res <- NULL
      e <- try({
        fit <- lm(formula, data = df)
        r <- resid(fit)
        res <- broom::tidy(shapiro.test(r))
        res$`Residuals normal distributed` <- res$p.value > 0.05
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
      df <- data$df
      req(is.data.frame(df))
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      fit <- NULL
      e <- try({
        fit <- broom::tidy(car::leveneTest(formula, data = df, center = input$center))
        fit$`Variance homogenity` <- fit$p.value > 0.05
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
        if (!inherits(listResults$curr_data, "plot")) {
          return(listResults$curr_data)
        }
        return(NULL)
      },
      digits = 6
    )

    runDiagnosticPlot <- function() {
      output$ass_error <- renderText(NULL)
      df <- data$df
      req(is.data.frame(df))
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      p <- NULL
      e <- try({
        p <- diagnosticPlots(df, formula)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$ass_error <- renderText(err)
      } else {
        listResults$curr_data <- new("plot", p = p, width = 15, height = 15, resolution = 600)
        listResults$curr_name <- paste("Plot Nr", length(listResults$all_names) + 1, "diagnostic plots")
        output$DiagnosticPlotRes <- renderPlot(p)
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

  return(listResults)
}
