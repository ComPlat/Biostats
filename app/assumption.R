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
  fluidRow()
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

    output$open_formula_editor_corr <- renderUI({
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
        footer = tagList(
          modalButton("Close")
        )
      ))
    })

    # display current formula
    observe({
      req(!is.null(data$formula))
      output$formula <- renderText({deparse(data$formula)})
    })

    runShapiro <- function() {
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
        }, silent = TRUE)
        if (!inherits(e, "try-error")) {
          exportTestValues(
            assumption_res  = res
          )
          listResults$counter <- listResults$counter + 1
          new_name <- paste0(
            "ShapiroDataNr", listResults$counter
          )
          listResults$all_data[[new_name]] <- res
          output$curr_error <- renderText(err)
        } else {
          err <- conditionMessage(attr(e, "condition"))
          print_req(FALSE, err)
        }
      }
    }
    observeEvent(input$shapiro, {
      runShapiro()
    })

    runShapiroResiduals <- function() {
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
      }, silent = TRUE)
      if (!inherits(e, "try-error")) {
        exportTestValues(
          assumption_res  = res
        )
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "ShaprioResidualsNr", listResults$counter
        )
        listResults$all_data[[new_name]] <- res
        output$curr_error <- renderText(err)
      } else {
        err <- conditionMessage(attr(e, "condition"))
        print_req(FALSE, err)
      }
    }
    observeEvent(input$shapiroResiduals, {
      runShapiroResiduals()
    })

    runLevene <- function() {
      df <- data$df
      req(is.data.frame(df))
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      fit <- NULL
      e <- try({
        fit <- broom::tidy(car::leveneTest(formula, data = df, center = input$center))
        fit$`Variance homogenity` <- fit$p.value > 0.05
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_req(FALSE, err)
      } else {
        exportTestValues(
          assumption_res  = fit
        )
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "LeveneTestNr", listResults$counter
        )
        listResults$all_data[[new_name]] <- fit
        output$curr_error <- renderText(err)
      }
    }
    observeEvent(input$levene, {
      runLevene()
    })

    runDiagnosticPlot <- function() {
      df <- data$df
      req(is.data.frame(df))
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      p <- NULL
      e <- try({
        p <- diagnosticPlots(df, formula)
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_req(FALSE, err)
      } else {
        exportTestValues(
          assumption_res  = p
        )
        listResults$counter <- listResults$counter + 1
        new_result_name <- paste0("DiagnosticPlotNr", listResults$counter)
        listResults$all_data[[new_result_name]] <-
          new("plot", p = p, width = 15, height = 15, resolution = 600)
        output$DiagnosticPlotRes <- renderPlot(p)
        output$curr_error <- renderText(err)
      }
    }
    observeEvent(input$DiagnosticPlot, {
      runDiagnosticPlot()
    })

  })

  return(listResults)
}
