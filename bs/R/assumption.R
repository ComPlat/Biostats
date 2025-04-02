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
      formula <- data$formula
      check <- TRUE
      res <- NULL
      temp <- NULL
      err <- NULL
      if (isTRUE(check)) {
        res <- list()
        e <- try(
          {
            res <- withCallingHandlers(
              {
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
                check_rls(listResults$all_data, res)
                res
              },
              warning = function(warn) {
                print_warn(warn$message)
                invokeRestart("muffleWarning")
              }
            )
          },
          silent = TRUE
        )
        if (!inherits(e, "try-error")) {
          exportTestValues(
            assumption_res = res
          )
          listResults$counter <- listResults$counter + 1
          new_name <- paste0(
            "ShapiroDataNr", listResults$counter
          )
          listResults$all_data[[new_name]] <- res
          listResults$history[[length(listResults$history) + 1]] <- list(
            type = "ShapiroOnData",
            formula = deparse(data$formula),
            "Result name" = new_name
          )
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
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(data$formula)
      formula <- data$formula
      res <- NULL
      e <- try(
        {
          withCallingHandlers(
            {
              fit <- lm(formula, data = df)
              r <- resid(fit)
              res <- broom::tidy(shapiro.test(r))
              res$`Residuals normal distributed` <- res$p.value > 0.05
              check_rls(listResults$all_data, res)
              res
            },
            warning = function(warn) {
              print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
        },
        silent = TRUE
      )
      if (!inherits(e, "try-error")) {
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
        err <- conditionMessage(attr(e, "condition"))
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
      formula <- data$formula
      fit <- NULL
      e <- try(
        {
          withCallingHandlers(
            {
              fit <- broom::tidy(car::leveneTest(formula, data = df, center = input$center))
              fit$`Variance homogenity` <- fit$p.value > 0.05
              check_rls(listResults$all_data, fit)
              fit
            },
            warning = function(warn) {
              print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
        },
        silent = TRUE
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_err(err)
      } else {
        exportTestValues(
          assumption_res = fit
        )
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "LeveneTestNr", listResults$counter
        )
        listResults$all_data[[new_name]] <- fit
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "LeveneTest",
          formula = deparse(data$formula),
          center = input$center,
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
      formula <- data$formula
      p <- NULL
      e <- try(
        {
          withCallingHandlers(
            {
              p <- diagnosticPlots(df, formula)
              check_rls(listResults$all_data, p)
              p
            },
            warning = function(warn) {
              print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
        },
        silent = TRUE
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_err(err)
      } else {
        exportTestValues(
          assumption_res = p
        )
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

  return(listResults)
}
