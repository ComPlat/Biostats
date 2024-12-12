DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    div(
      style = "position: relative;",
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
      uiOutput(NS(id, "substanceNamesUI")),
      uiOutput(NS(id, "negIdentifierUI")),
      uiOutput(NS(id, "posIdentifierUI")),
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
        plotOutput(NS(id, "dr_result_plot")),
        actionButton(NS(id, "previousPage"), "Previous plot"),
        actionButton(NS(id, "nextPage"), "Next plot")
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
      currentPageOverview = 1
    )

    # Render names, conc and abs column
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

    output[["negIdentifierUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      req(input$`substanceNames`)
      choices <- unique(data$df[[input$substanceNames]])
      req(length(choices) >= 1)
      tooltip <- "Select the name used for the negative control"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("DOSERESPONSE-negIdentifier"),
          label = "Name of the negative control",
          choices = choices[1:length(choices)],
          selected = NULL
        )
      )
    })

    output[["posIdentifierUI"]] <- renderUI({
      req(!is.null(data$df))
      req(is.data.frame(data$df))
      req(input$`substanceNames`)
      choices <- unique(data$df[[input$substanceNames]])
      req(length(choices) >= 1)
      tooltip <- "Select the name used for the positive control"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("DOSERESPONSE-posIdentifier"),
          label = "Name of the positive control",
          choices = choices[1:length(choices)],
          selected = NULL
        )
      )
    })

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
      output$formula <- renderText({
        deparse(data$formula)
      })
    })

    drFct <- function() {
      req(is.data.frame(data$df))
      df <- data$df
      req(input$substanceNames)
      names <- input$substanceNames
      req(input$negIdentifier)
      neg <- input$negIdentifier
      req(input$posIdentifier)
      pos <- input$posIdentifier
      print_req(!is.null(data$formula), "You have to set a formula")
      req(!is.null(data$formula))
      r_vals$plots <- NULL # reset
      r_vals$names <- NULL # reset
      r_vals$currentPage <- 1 # reset
      r_vals$overview_plots <- NULL # reset
      r_vals$currentPageOverview <- 1 # reset
      f <- as.character(data$formula)
      dep <- f[2]
      indep <- f[3]
      err <- NULL
      resDF <- NULL
      resPlot <- NULL
      e <- try({
        check_ast(str2lang(indep), colnames(df))
        check_ast(str2lang(dep), colnames(df))
        res <- ic50(df, dep, indep, names, neg, pos)
        stopifnot(!inherits(res, "errorClass"))
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
        r_vals$plots <- resP
        r_vals$names <- resDF$name
        resPlot <- resP
        overviewPlots <- create_plot_pages(resPlot)
        r_vals$overview_plots <- overviewPlots
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_req(FALSE, err)
      } else {
        listResults$curr_data <- new("doseResponse", df = resDF, p = resPlot)
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted dose response analysis")
        output$dr_result <- renderTable(resDF, digits = 6)

        listResults$counter <- listResults$counter + 1
        new_result_name <- paste0("DoseResponseNr", listResults$counter)
        listResults$all_data[[new_result_name]] <- new("doseResponse", df = resDF, p = resPlot)

        exportTestValues(
          doseresponse_res = listResults$curr_data
        )
      }
    }

    observeEvent(input$ic50, {
      drFct()
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
