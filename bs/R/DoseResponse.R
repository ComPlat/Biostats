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

DoseResponseServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {
    r_vals <- reactiveValues(
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
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      colnames <- names(data$df)
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
      r_vals$plots <- NULL
      r_vals$names <- NULL
      r_vals$currentPage <- 1
      r_vals$overview_plots <- NULL
      r_vals$currentPageOverview <- 1
      r_vals$outliers <- NULL
      r_vals$df_dr <- NULL
    }

    check_dr <- function() {
      print_req(is.data.frame(data$df), "The dataset is missing")
      req(input$substanceNames)
      print_form(data$formula)
      req(!is.null(data$formula))
    }

    run_dr <- function(df, outliers) {
      is_xlog <- input$xTransform
      is_ylog <- input$yTransform
      names <- input$substanceNames
      f <- as.character(data$formula)
      formula <- data$formula
      dep <- f[2]
      indep <- f[3]
      resDF <- NULL
      resP <- NULL
      err <- try({
        check_formula(formula)
        check_ast(str2lang(indep), colnames(df))
        check_ast(str2lang(dep), colnames(df))
        res <- ic50(
          df, dep,
          indep, names, outliers,
          is_xlog, is_ylog
        )
        if (inherits(res, "errorClass")) {
          stop(res$error_message)
        }
        resDF <- lapply(res, function(x) {
          if (inherits(x, "errorClass")) {
            return(NULL)
          }
          return(x[[1]])
        })
        resDF <- resDF[!is.null(resDF)]
        resDF <- resDF[!sapply(resDF, is.null)]
        resDF <- Reduce(rbind, resDF)
        resP <- lapply(res, function(x) {
          if (inherits(x, "errorClass")) {
            return(NULL)
          }
          return(x[[2]])
        })
        resP <- resP[!is.null(resP)]
        resP <- resP[!sapply(resP, is.null)]
      })
      if (inherits(err, "try-error")) {
        return(err)
      }
      return(list(resDF, resP))
    }

    dr_complete <- function() {
      check_dr()
      df <- data$df
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
          r_vals$plots <- resP
          r_vals$names <- resDF$name
          r_vals$df_dr <- resDF
          overviewPlots <- create_plot_pages(resP)
          r_vals$overview_plots <- overviewPlots
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
        listResults$curr_data <- new("doseResponse", df = resDF, p = resP)
        listResults$curr_name <- paste(
          "Test Nr", length(listResults$all_names) + 1,
          "Conducted dose response analysis"
        )
        listResults$counter <- listResults$counter + 1
        new_result_name <- paste0("DoseResponseNr", listResults$counter)
        listResults$all_data[[new_result_name]] <- new(
          "doseResponse",
          df = resDF, p = resP
        )
        exportTestValues(
          doseresponse_res = listResults$curr_data
        )
      }
    }

    observeEvent(input$ic50, {
      dr_complete()
    })

    # Display plots
    observe({
      req(!is.null(r_vals$plots))
      req(is.list(r_vals$plots))
      output$dr_result_plot <- renderPlot(r_vals$plots[[r_vals$currentPage]])
    })

    output[["dropdown_plots"]] <- renderUI({
      req(!is.null(r_vals$plots))
      req(is.list(r_vals$plots))
      req(length(r_vals$plots) > 1)
      req(!is.null(r_vals$names))
      req(length(r_vals$names) > 1)
      selectInput(
        inputId = "DOSERESPONSE-dropdown_plots",
        label = "Select plot",
        choices = r_vals$names,
        selected = r_vals$names[1]
      )
    })

    observeEvent(input$dropdown_plots, {
      req(!is.null(r_vals$plots))
      req(is.list(r_vals$plots))
      req(length(r_vals$plots) > 1)
      req(!is.null(r_vals$names))
      req(length(r_vals$names) > 1)
      r_vals$currentPage <- which(input$dropdown_plots == r_vals$names)
    })

    observeEvent(input$nextPage, {
      req(!is.null(r_vals$plots))
      req(is.list(r_vals$plots))
      req(length(r_vals$plots) > 1)
      if (r_vals$currentPage < length(r_vals$plots)) {
        r_vals$currentPage <- r_vals$currentPage + 1
      }
    })

    observeEvent(input$previousPage, {
      req(!is.null(r_vals$plots))
      req(is.list(r_vals$plots))
      req(length(r_vals$plots) > 1)
      if (r_vals$currentPage > 1) {
        r_vals$currentPage <- r_vals$currentPage - 1
      }
    })

    dr_partial <- function(df, name) {
      check_dr()
      resDF <- NULL
      resP <- NULL
      e <- try(
        {
          outliers <- list(r_vals$outliers[[name]])
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
          names <- r_vals$names
          idx <- which(name == names)
          resDF <- res[[1]]
          resP <- res[[2]][[1]]
          old_plots <- r_vals$plots
          old_df_dr <- r_vals$df_dr
          old_plots[[idx]] <- resP
          old_df_dr[idx, ] <- resDF
          r_vals$plots <- old_plots
          r_vals$df_dr <- old_df_dr
          resP <- r_vals$plots
          resDF <- r_vals$df_dr
          overviewPlots <- create_plot_pages(resP)
          r_vals$overview_plots <- overviewPlots
          check_rls(listResults$all_data, res)
        },
        silent = TRUE
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$dr_result <- renderTable(data.frame(), digits = 6)
        print_err(err)
      } else {
        # TODO: add version for Substance. _S4_V1, _S4_V2 ...
        # Why does it jump after update?
        output$dr_result <- renderTable(resDF, digits = 6)
        listResults$curr_data <- new("doseResponse", df = resDF, p = resP)
        listResults$curr_name <- paste(
          "Test Nr", length(listResults$all_names) + 1,
          "Conducted dose response analysis"
        )
        listResults$counter <- listResults$counter + 1
        new_result_name <- paste0("DoseResponseNr", listResults$counter)
        listResults$all_data[[new_result_name]] <- new(
          "doseResponse",
          df = resDF, p = resP
        )
        exportTestValues(
          doseresponse_res = listResults$curr_data
        )
      }
    }

    observeEvent(input$plot_click, {
      req(!is.null(input$plot_click))
      req(is.data.frame(data$df))
      print_req(!is.null(data$formula), "You have to set a formula")
      try({
        click <- input$plot_click
        name_col <- input$substanceNames
        df <- data$df
        sub_df <- df[df[, name_col] == r_vals$names[r_vals$currentPage], ]
        f <- as.character(data$formula)
        formula <- data$formula
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
          name <- r_vals$names[r_vals$currentPage]
          old_outliers <- r_vals$outliers[[name]]
          if (is.null(old_outliers)) {
            r_vals$outliers[[name]] <- nearest
          } else {
            if (nearest %in% old_outliers) {
              old_outliers <- old_outliers[old_outliers != nearest]
            } else {
              old_outliers <- c(old_outliers, nearest)
            }
            r_vals$outliers[[name]] <- old_outliers
          }
          old_current_page <- r_vals$currentPage
          dr_partial(sub_df, name)
          r_vals$currentPage <- old_current_page
        }
      })
    })

    # Display overview plots
    observe({
      req(!is.null(r_vals$overview_plots))
      req(is.list(r_vals$overview_plots))
      output$dr_overview_plot <- renderPlot(
        r_vals$overview_plots[[r_vals$currentPageOverview]]
      )
    })

    observeEvent(input$nextPageOverview, {
      req(!is.null(r_vals$overview_plots))
      req(is.list(r_vals$overview_plots))
      req(length(r_vals$overview_plots) > 1)
      if (r_vals$currentPageOverview < length(r_vals$overview_plots)) {
        r_vals$currentPageOverview <- r_vals$currentPageOverview + 1
      }
    })

    observeEvent(input$previousPageOverview, {
      req(!is.null(r_vals$overview_plots))
      req(is.list(r_vals$overview_plots))
      req(length(r_vals$overview_plots) > 1)
      if (r_vals$currentPageOverview > 1) {
        r_vals$currentPageOverview <- r_vals$currentPageOverview - 1
      }
    })
  })

  return(listResults)
}
