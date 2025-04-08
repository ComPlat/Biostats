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

assServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {

    runShapiro <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        sod <- shapiro_on_data$new(DataModelState$df,DataModelState$formula)
        sod$validate()
        sod$eval(ResultsState)
      })
      if (!inherits(res, "try-error")) {
        exportTestValues(
          assumption_res = res
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
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        sor <- shapiro_on_residuals$new(DataModelState$df,DataModelState$formula)
        sor$validate()
        sor$eval(ResultsState)
      }, silent = TRUE)

      if (!inherits(res, "try-error")) {
        exportTestValues(
          assumption_res = res
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
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        l <- levene$new(DataModelState$df,DataModelState$formula, input$center)
        l$validate()
        l$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      } else {
        exportTestValues(
          assumption_res = res
        )
      }
    }
    observeEvent(input$levene, {
      runLevene()
    })

    runDiagnosticPlot <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      p <- try({
        dp <- diagnostic_plots$new(DataModelState$df,DataModelState$formula)
        dp$validate()
        dp$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(p, "try-error")) {
        err <- conditionMessage(attr(p, "condition"))
        print_err(err)
      } else {
        output$DiagnosticPlotRes <- renderPlot(p)
        exportTestValues(assumption_res = p)
      }
    }
    observeEvent(input$DiagnosticPlot, {
      runDiagnosticPlot()
    })
  })

}
