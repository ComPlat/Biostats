assSidebarUI <- function(id) {
  tabPanel(
    "Assumption",
    tags$hr(),
    uiOutput(NS(id, "shapiro")),
    tags$hr(),
    uiOutput(NS(id, "shapiroResidualsUI")),
    tags$hr(),
    uiOutput(NS(id, "LeveneUI")),
    tags$hr(),
    uiOutput(NS(id, "DiagnosticPlot"))
  )
}

assUI <- function(id) {
  fluidRow()
}

# TODO: glm
# 1. Assumptions for the ratio = mean/var analysis; Dispersion is the factor the variance is larger than expected
# 2. Residuen analysis; residual plots on response scale and link scale
# 3. relationship of response and predictors; Complicated
#
# TODO: Show all assumptions
# Show all assumption tests even when no formula is set in the same way it is handled in the tests tab

assServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {

    # React to model type
    output[["shapiro"]] <- renderUI({
      if (is.null(DataModelState$formula)) {
        return(
          div(
            class = "var-box-output",
            h3(strong("You have to define a model in the formula editor to run any assumptions tests"))
          )
        )
      }
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if(inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        div(
          h4(strong("Test of normal distribution")),
          hr(),
          actionButton("ASS-shapiro",
            "Shapiro test for individual groups",
            title =
            "Use this test if you have a formula like 'response ~ pred1 * pred2' (two-way ANOVA) to check normality of residuals within each group."
          ),
          br()
        )
      } else if (inherits(DataModelState$formula, "OptimFormula")) {
        div(
          class = "var-box-output",
          h3(strong("There are no meaningful tests for an optimization"))
        )
      }
    })
    output[["shapiroResidualsUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      req(DataModelState$formula)
      if(inherits(DataModelState$formula, "LinearFormula")) {
        actionButton("ASS-shapiroResiduals", "Shapiro test for residuals of linear model",
          title =
          "Use this test if you have a formula like 'response ~ predictor1' to check normality of the residuals of the linear model."
        )
      }
    })
    output[["LeveneUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      req(DataModelState$formula)
      if(inherits(DataModelState$formula, "LinearFormula")) {
        div(
          hr(),
          div(
            class = "header", checked = NA,
            h4(
              style = "font-weight: bold;",
              "Test of variance homogenity"
            )
          ),
          actionButton(NS(id, "levene"), "Levene test"), # NOTE: using ASS-levene is in this case wrong dont know why?
          selectInput(NS(id, "center"), "Data center of each group: mean or median", # The same is true for center
            c(
              "Mean" = "mean",
              "Median" = "median"
            ),
            selectize = FALSE
          )
        )
      }
    })
    output[["DiagnosticPlot"]] <- renderUI({
      if(inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        div(
          div(
            class = "header", checked = NA,
            h4(style = "font-weight: bold;", "Visual tests")
          ),
          actionButton("ASS-DiagnosticPlot", "diagnostic plots")
        )
      }
    })

    runShapiro <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        sod <- shapiro_on_data_V1_2$new(DataModelState$df,DataModelState$formula)
        sod$validate()
        sod$eval(ResultsState)
      })
      if (inherits(res, "try-error")) {
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
        sor <- shapiro_on_residuals_V1_2$new(DataModelState$df,DataModelState$formula)
        sor$validate()
        sor$eval(ResultsState)
      }, silent = TRUE)

      if (inherits(res, "try-error")) {
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
        l <- levene_V1_2$new(DataModelState$df,DataModelState$formula, input$center)
        l$validate()
        l$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
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
        dp <- diagnostic_plots_V1_2$new(DataModelState$df, DataModelState$formula)
        dp$validate()
        dp$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(p, "try-error")) {
        err <- conditionMessage(attr(p, "condition"))
        print_err(err)
      }
    }
    observeEvent(input$DiagnosticPlot, {
      runDiagnosticPlot()
    })
  })

}
