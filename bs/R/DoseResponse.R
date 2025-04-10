DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    div(
      style = "position: relative;",
      br(),
      uiOutput(NS(id, "substanceNamesUI")),
      checkboxInput(
        NS(id, "xTransform"),
        label = "Log transform x-axis",
        value = FALSE
      ),
      checkboxInput(
        NS(id, "yTransform"),
        label = "Log transform y-axis",
        value = FALSE
      ),
      actionButton(NS(id, "ic50"), "Conduct analysis")
    )
  )
}

DoseResponseUI <- function(id) {
  fluidRow(
    tabsetPanel(
      id = NS(id, "results_tabs"),
      tabPanel(
        "Results Table",
        tableOutput(NS(id, "dr_result"))
      ),
      tabPanel(
        "Overview Plot",
        plotOutput(NS(id, "dr_overview_plot"), height = 700),
        actionButton(NS(id, "previousPageOverview"), "Previous page"),
        actionButton(NS(id, "nextPageOverview"), "Next page")
      ),
      tabPanel(
        "Results Plot",
        uiOutput(NS(id, "dropdown_plots")),
        plotOutput(
          NS(id, "dr_result_plot"),
          click = NS(id, "plot_click")
        ),
        actionButton(NS(id, "previousPage"), "Previous plot"),
        actionButton(NS(id, "nextPage"), "Next plot"),
        p("Click on the plot to mark outliers")
      )
    )
  )
}

DoseResponseServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {
    DoseResponseState <- reactiveValues(
      plots = NULL,
      names = NULL, # For dropdown_plots
      overview_plots = NULL,
      currentPage = 1,
      currentPageOverview = 1,
      outliers = NULL,
      df_dr = NULL
    )

    # Render names of substances
    output[["substanceNamesUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- names(DataModelState$df)
      tooltip <- "Select the column which contains the names of the different substances"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("DOSERESPONSE-substanceNames"),
          label = "Column containing the names",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    reset_dr <- function() {
      DoseResponseState$plots <- NULL
      DoseResponseState$names <- NULL
      DoseResponseState$currentPage <- 1
      DoseResponseState$overview_plots <- NULL
      DoseResponseState$currentPageOverview <- 1
      DoseResponseState$outliers <- NULL
      DoseResponseState$df_dr <- NULL
    }

    check_dr <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      req(input$substanceNames)
      print_form(DataModelState$formula)
      req(!is.null(DataModelState$formula))
    }

    run_dr <- function(df, outliers) {
      dr <- dose_response$new(
        df, outliers, input$xTransform, input$yTransform,
        input$substanceNames, DataModelState$formula
      )
      dr$eval(NULL)
    }

    dr_complete <- function() {
      check_dr()
      df <- DataModelState$df
      reset_dr()
      resDF <- NULL
      resP <- NULL
      e <- try(
        {
          res <- run_dr(df, NULL)
          if (inherits(res, "try-error")) {
            m <- conditionMessage(attr(res, "condition"))
            stop(m)
          }
          resDF <- res[[1]]
          resP <- res[[2]]
          DoseResponseState$plots <- resP
          DoseResponseState$names <- resDF$name
          DoseResponseState$df_dr <- resDF
          overviewPlots <- create_plot_pages(resP)
          DoseResponseState$overview_plots <- overviewPlots
        },
        silent = TRUE
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        reset_dr()
        output$dr_result <- renderTable(data.frame(), digits = 6)
        print_err(err)
      } else {
        output$dr_result <- renderTable(resDF, digits = 6)
        ResultsState$curr_data <- new("doseResponse", df = resDF, p = resP, outlier_info = "")
        ResultsState$curr_name <- paste(
          "Test Nr", length(ResultsState$all_names) + 1,
          "Conducted dose response analysis"
        )
        ResultsState$counter <- ResultsState$counter + 1
        new_result_name <- paste0("DoseResponseNr", ResultsState$counter)
        ResultsState$all_data[[new_result_name]] <- new(
          "doseResponse",
          df = resDF, p = resP, outlier_info = ""
        )
        exportTestValues(
          doseresponse_res = ResultsState$curr_data
        )
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "DoseResponse",
          "Column containing the names" = input$substanceNames,
          "Log transform x-axis" = input$xTransform,
          "Log transform y-axis" = input$yTransform,
          "formula" = deparse(DataModelState$formula),
          "Result name" = new_result_name
        )
      }
    }

    observeEvent(input$ic50, {
      dr_complete()
    })

    # Display plots
    observe({
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      output$dr_result_plot <- renderPlot(DoseResponseState$plots[[DoseResponseState$currentPage]])
    })

    output[["dropdown_plots"]] <- renderUI({
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      req(!is.null(DoseResponseState$names))
      req(length(DoseResponseState$names) > 1)
      selectInput(
        inputId = "DOSERESPONSE-dropdown_plots",
        label = "Select plot",
        choices = DoseResponseState$names,
        selected = DoseResponseState$names[1]
      )
    })

    observeEvent(input$dropdown_plots, {
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      req(!is.null(DoseResponseState$names))
      req(length(DoseResponseState$names) > 1)
      DoseResponseState$currentPage <- which(input$dropdown_plots == DoseResponseState$names)
    })

    observeEvent(input$nextPage, {
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      if (DoseResponseState$currentPage < length(DoseResponseState$plots)) {
        DoseResponseState$currentPage <- DoseResponseState$currentPage + 1
      }
    })

    observeEvent(input$previousPage, {
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      if (DoseResponseState$currentPage > 1) {
        DoseResponseState$currentPage <- DoseResponseState$currentPage - 1
      }
    })

    dr_partial <- function(df, name) {
      check_dr()
      resDF <- NULL
      resP <- NULL
      outliers <- NULL
      e <- try(
        {
          outliers <- list(DoseResponseState$outliers[[name]])
          names(outliers) <- name
          if (length(outliers[[name]]) == 0) {
            outliers <- list(NULL)
            names(outliers) <- name
          }
          res <- run_dr(df, outliers)
          if (inherits(res, "try-error")) {
            m <- conditionMessage(attr(res, "condition"))
            stop(m)
          }
          names <- DoseResponseState$names
          idx <- which(name == names)
          resDF <- res[[1]]
          resP <- res[[2]][[1]]
          old_plots <- DoseResponseState$plots
          old_df_dr <- DoseResponseState$df_dr
          old_plots[[idx]] <- resP
          old_df_dr[idx, ] <- resDF
          DoseResponseState$plots <- old_plots
          DoseResponseState$df_dr <- old_df_dr
          resP <- DoseResponseState$plots
          resDF <- DoseResponseState$df_dr
          overviewPlots <- create_plot_pages(resP)
          DoseResponseState$overview_plots <- overviewPlots
          check_rls(ResultsState$all_data, res)
        },
        silent = TRUE
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$dr_result <- renderTable(data.frame(), digits = 6)
        print_err(err)
      } else {
        output$dr_result <- renderTable(resDF, digits = 6)
        ResultsState$curr_data <- new(
          "doseResponse",
          df = resDF, p = resP, outlier_info = create_outlier_info(DoseResponseState$outliers)
        )
        ResultsState$curr_name <- paste(
          "Test Nr", length(ResultsState$all_names) + 1,
          "Conducted dose response analysis"
        )
        ResultsState$counter <- ResultsState$counter + 1
        new_result_name <- paste0("DoseResponseNr", ResultsState$counter)
        ResultsState$all_data[[new_result_name]] <- new(
          "doseResponse",
          df = resDF, p = resP, outlier_info = create_outlier_info(DoseResponseState$outliers)
        )
        exportTestValues(
          doseresponse_res = ResultsState$curr_data
        )
        outliers <- list(DoseResponseState$outliers[[name]])
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "DoseResponse",
          "Column containing the names" = input$substanceNames,
          "Log transform x-axis" = input$xTransform,
          "Log transform y-axis" = input$yTransform,
          "formula" = deparse(DataModelState$formula),
          outliers = create_outlier_info(DoseResponseState$outliers),
          "Result name" = new_result_name
        )
      }
    }

    observeEvent(input$plot_click, {
      req(!is.null(input$plot_click))
      req(is.data.frame(DataModelState$df))
      print_req(!is.null(DataModelState$formula), "You have to set a formula")
      try({
        click <- input$plot_click
        name_col <- input$substanceNames
        df <- DataModelState$df
        sub_df <- df[df[, name_col] == DoseResponseState$names[DoseResponseState$currentPage], ]
        f <- as.character(DataModelState$formula)
        formula <- DataModelState$formula
        check_formula(formula)
        dep <- f[2]
        indep <- f[3]
        x <- as.numeric(sub_df[, indep])
        y <- as.numeric(sub_df[, dep])
        x <- x[!is.na(x)]
        y <- y[!is.na(y)]
        x_range <- range(x, na.rm = TRUE)
        y_range <- range(y, na.rm = TRUE)
        x_normalized <- (x - x_range[1]) / (x_range[2] - x_range[1])
        y_normalized <- (y - y_range[1]) / (y_range[2] - y_range[1])
        click_x_normalized <- (click$x - x_range[1]) / (x_range[2] - x_range[1])
        click_y_normalized <- (click$y - y_range[1]) / (y_range[2] - y_range[1])
        distances <- sqrt((x_normalized - click_x_normalized)^2 + (y_normalized - click_y_normalized)^2)
        nearest <- which.min(distances)
        if (distances[nearest] < 0.1) {
          name <- DoseResponseState$names[DoseResponseState$currentPage]
          old_outliers <- DoseResponseState$outliers[[name]]
          if (is.null(old_outliers)) {
            DoseResponseState$outliers[[name]] <- nearest
          } else {
            if (nearest %in% old_outliers) {
              old_outliers <- old_outliers[old_outliers != nearest]
            } else {
              old_outliers <- c(old_outliers, nearest)
            }
            DoseResponseState$outliers[[name]] <- old_outliers
          }
          old_current_page <- DoseResponseState$currentPage
          dr_partial(sub_df, name)
          DoseResponseState$currentPage <- old_current_page
        }
      })
    })

    # Display overview plots
    observe({
      req(!is.null(DoseResponseState$overview_plots))
      req(is.list(DoseResponseState$overview_plots))
      output$dr_overview_plot <- renderPlot(
        DoseResponseState$overview_plots[[DoseResponseState$currentPageOverview]]
      )
    })

    observeEvent(input$nextPageOverview, {
      req(!is.null(DoseResponseState$overview_plots))
      req(is.list(DoseResponseState$overview_plots))
      req(length(DoseResponseState$overview_plots) > 1)
      if (DoseResponseState$currentPageOverview < length(DoseResponseState$overview_plots)) {
        DoseResponseState$currentPageOverview <- DoseResponseState$currentPageOverview + 1
      }
    })

    observeEvent(input$previousPageOverview, {
      req(!is.null(DoseResponseState$overview_plots))
      req(is.list(DoseResponseState$overview_plots))
      req(length(DoseResponseState$overview_plots) > 1)
      if (DoseResponseState$currentPageOverview > 1) {
        DoseResponseState$currentPageOverview <- DoseResponseState$currentPageOverview - 1
      }
    })
  })

  return(ResultsState)
}
