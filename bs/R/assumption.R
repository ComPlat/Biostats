assSidebarUI <- function(id) {
  tabPanel(
    "Assumption",
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
        "Use this test if you have a formula like 'response ~ pred1 * pred2' (two-way ANOVA) to check normality of residuals within each group."
    ),
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

    runShapiro <- function() {
      df <- data$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(data$formula)
      sod <- shapiro_on_data$new(data$df, data$formula)
      res <- try({sod$eval(listResults)})

      if (!inherits(res, "try-error")) {
        exportTestValues(
          assumption_res = res
        )
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "ShapiroDataNr", listResults$counter
        )
        listResults$all_data[[new_name]] <-res
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "ShapiroOnData",
          formula = deparse(data$formula),
          "Result name" = new_name
        )
      } else {
        err <- conditionMessage(attr(res, "condition"))
        print_req(FALSE, err)
      }
    }

    observeEvent(input$shapiro, {
      runShapiro()
    })

    runShapiroResiduals <- function() {
      df <- data$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(data$formula)
      sor <- shapiro_on_residuals$new(data$df, data$formula)
      res <- try({ sor$eval(listResults) }, silent = TRUE)
      if (!inherits(res, "try-error")) {
        exportTestValues(
          assumption_res = res
        )
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "ShaprioResidualsNr", listResults$counter
        )
        listResults$all_data[[new_name]] <- res
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "shapiroOnResiduals",
          formula = deparse(data$formula),
          "Result name" = new_name
        )
      } else {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      }
    }
    observeEvent(input$shapiroResiduals, {
      runShapiroResiduals()
    })

    runLevene <- function() {
      df <- data$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(data$formula)
      l <- levene$new(data$df, data$formula, input$center)
      res <- try( { l$eval(listResults) }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      } else {
        exportTestValues(
          assumption_res = res
        )
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "LeveneTestNr", listResults$counter
        )
        listResults$all_data[[new_name]] <- res
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "LeveneTest",
          formula = deparse(data$formula),
          "Data center" = input$center,
          "Result name" = new_name
        )
      }
    }
    observeEvent(input$levene, {
      runLevene()
    })

    runDiagnosticPlot <- function() {
      df <- data$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(data$formula)
      dp <- diagnostic_plots$new(data$df, data$formula)
      p <- try( { dp$eval(listResults) }, silent = TRUE)
      if (inherits(p, "try-error")) {
        err <- conditionMessage(attr(p, "condition"))
        print_err(err)
      } else {
        exportTestValues( assumption_res = p)
        listResults$counter <- listResults$counter + 1
        new_result_name <- paste0("DiagnosticPlotNr", listResults$counter)
        listResults$all_data[[new_result_name]] <-
          new("plot", p = p, width = 15, height = 15, resolution = 600)
        output$DiagnosticPlotRes <- renderPlot(p)
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "DiagnosticPlots",
          formula = deparse(data$formula),
          "Result name" = new_result_name
        )
      }
    }
    observeEvent(input$DiagnosticPlot, {
      runDiagnosticPlot()
    })
  })

}
