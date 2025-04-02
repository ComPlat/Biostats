corrSidebarUI <- function(id) {
  tabPanel(
    "Correlation",
    br(),
    sliderInput(NS(id, "conflevel"), "Confidence level of the interval",
      min = 0, max = 1, value = 0.95
    ),
    selectInput(
      NS(id, "alt"), "Alternative hypothesis",
      c(
        "Two sided" = "two.sided",
        "Less" = "less",
        "Greater" = "greater"
      )
    ),
    actionButton(NS(id, "pear"), "Pearson correlation",
      title =
        "Measures the linear relationship between two continuous variables. Assumes normal distribution and equal variance."
    ),
    actionButton(NS(id, "spear"), "Spearman correlation",
      title =
        "Measures the monotonic relationship between two variables using ranks. Suitable for ordinal data or non-linear relationships."
    ),
    actionButton(NS(id, "kendall"), "Kendall correlation",
      title =
        "Measures the strength of dependence between two variables based on rank concordance. Works well with small samples or tied ranks."
    )
  )
}

corrUI <- function(id) {
  fluidRow()
}

corrServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {

    corr_fct <- function(method) {
      print_req(is.data.frame(data$df), "The dataset is missing")
      print_form(data$formula)
      f <- as.character(data$formula)
      dep <- f[2]
      indep <- f[3]
      d <- data$df
      tryCatch(
        {
          check_ast(str2lang(indep), colnames(df)) # NOTE: check_ast throws error
          check_ast(str2lang(dep), colnames(df))
          fit <- withCallingHandlers(
            expr = broom::tidy(
              cor.test(d[, dep], d[, indep],
                method = method,
                alternative = input$alt,
                conf.level = input$conflevel
              )
            ),
            warning = function(warn) {
              print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
          exportTestValues(
            correlation_res = fit
          )
          check_rls(listResults$all_data, fit)
          listResults$counter <- listResults$counter + 1
          new_name <- paste0(
            "Correlation", method, "NR", listResults$counter
          )
          listResults$all_data[[new_name]] <- fit
          listResults$history[[length(listResults$history) + 1]] <- list(
            type = "Correlation",
            formula = deparse(data$formula),
            method = method,
            alternative = input$alt,
            conf.level = input$conflevel,
            "Result name" = new_name
          )
        },
        error = function(err) {
          err <- err$message
          print_err(err)
        }
      )
    }

    observeEvent(input$pear, {
      corr_fct("pearson")
    })

    observeEvent(input$spear, {
      corr_fct("spearman")
    })

    observeEvent(input$kendall, {
      corr_fct("kendall")
    })
  })

  return(listResults)
}
