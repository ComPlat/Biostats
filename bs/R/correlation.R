corrSidebarUI <- function(id) {
  tabPanel(
    "Correlation",
    div(
      class = "boxed-output",
      uiOutput(NS(id, "open_formula_editor_corr")),
      verbatimTextOutput(NS(id, "formula"))
    ),
    br(),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "open_split_by_group")),
      uiOutput(NS(id, "data_splitted")),
      verbatimTextOutput(NS(id, "applied_filter"))
    ),
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

    # render formula button
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
      output$formula <- renderText({
        deparse(data$formula)
      })
    })

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
          listResults$counter <- listResults$counter + 1
          new_name <- paste0(
            "Correlation", method, "NR", listResults$counter
          )
          listResults$all_data[[new_name]] <- fit
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
