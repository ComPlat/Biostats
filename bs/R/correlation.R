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

corrServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {

    corr_fct <- function(method) {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form( DataModelState$formula)
      corr <- correlation$new(DataModelState$df, DataModelState$formula,
        method, input$alt, input$conflevel)
      tryCatch(
        {
          corr$validate()
          fit <- corr$eval(ResultsState)
          exportTestValues(
            correlation_res = fit
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

  return(ResultsState)
}
