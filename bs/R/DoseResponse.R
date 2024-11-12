# TODO: add everywhere the ? documentation.
# In an analogous way to the DoseResponse tab
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
      uiOutput(NS(id, "substanceNames")),
      uiOutput(NS(id, "negIdentifier")),
      uiOutput(NS(id, "posIdentifier")),
      actionButton(NS(id, "ic50"), "Conduct analysis")
    )
  )
}

DoseResponseUI <- function(id) {
  fluidRow(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "download.js")
    ),
    h4(strong("Results of test:")),
    actionButton(NS(id, "dr_save"), "Add output to result-file"),
    actionButton(NS(id, "download_dr"), "Save results"),
    checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL),
    tabsetPanel(
      id = NS(id, "results_tabs"),
      tabPanel("Results Table",
               tableOutput(NS(id, "dr_result"))),
      tabPanel("Results Plot",
        plotOutput(NS(id, "dr_result_plot")),
        actionButton(NS(id, "previousPage"), "Previous plot"),
        actionButton(NS(id, "nextPage"), "Next plot")
      )
    ),
    verbatimTextOutput(NS(id, "dr_error"))
  )
}

DoseResponseServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {

    r_vals <- reactiveValues(
      plots = NULL,
      currentPage = 1
    )

    # Render names, conc and abs column
    output[["substanceNames"]] <- renderUI({
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

    output[["negIdentifier"]] <- renderUI({
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
          choices = choices[1:length( choices)],
          selected = NULL
        )
      )
    })

    output[["posIdentifier"]] <- renderUI({
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
          choices = choices[1:length( choices)],
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
        footer = NULL
      ))
    })

    # display current formula
    observe({
      req(!is.null(data$formula))
      output$formula <- renderText({deparse(data$formula)})
    })

    drFct <- function() {
      output$dr_error <- renderText(NULL)
      req(is.data.frame(data$df))
      df <- data$df
      req(input$substanceNames)
      names <- input$substanceNames
      req(input$negIdentifier)
      neg <- input$negIdentifier
      req(input$posIdentifier)
      pos <- input$posIdentifier
      print_noti(!is.null(data$formula), "You have to set a formula")
      req(!is.null(data$formula))
      r_vals$plots <- NULL # reset
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
        resPlot <- resP
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$dr_error <- renderText(err)
      } else {
        listResults$curr_data <- new("doseResponse", df = resDF, p = resPlot)
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted dose response analysis")
        output$dr_result <- renderTable(resDF, digits = 6)
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

    observeEvent(input$dr_save, {
      if (is.null(listResults$curr_name)) {
        return(NULL)
      }
      if (!(listResults$curr_name %in% unlist(listResults$all_names))) {
        listResults$all_data[[length(listResults$all_data) + 1]] <- listResults$curr_data
        listResults$all_names[[length(listResults$all_names) + 1]] <- listResults$curr_name
      }
      updateCheckboxGroupInput(session, "TableSaved",
        choices = listResults$all_names
      )
    })

    observeEvent(input$download_dr, {
      lr <- unlist(listResults$all_names)
      indices <- sapply(input$TableSaved, function(x) {
        which(x == lr)
      })
      req(length(indices) >= 1)
      l <- listResults$all_data[indices]
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        excelFile <- createExcelFile(l)
        upload(session, excelFile, new_name = "Results.xlsx") # TODO: add possibility for desired file name
      } else {
        jsString <- createJSString(l)
        session$sendCustomMessage(
          type = "downloadZip",
          list(
            numberOfResults = length(jsString),
            FileContent = jsString
          )
        )
      }
    })
  })

  return(listResults)
}
