# TODO: add everywhere the ? documentation.
# In an analogous way to the DoseResponse tab
DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    div(
      style = "position: relative;",
      actionButton(
        NS(id, "df_help_icon"),
        label = NULL,
        icon = icon("question-circle"),
        style = "position: absolute; top: 10px; right: 10px; z-index: 1000;"
      ),
      uiOutput(NS(id,"open_formula_editor_corr")),
      br(),
      div(
        class = "boxed-output",
        uiOutput(NS(id, "open_split_by_group")),
        uiOutput(NS(id, "data_splitted")),
        verbatimTextOutput(NS(id, "applied_filter"))
      ),
      br(),
      textInput(NS(id, "substanceNames"), "Names column of dependent Variable", value = "names"),
      textInput(NS(id, "negIdentifier"), "Identifier for the negative control", value = "neg"),
      textInput(NS(id, "posIdentifier"), "Identifier for the positive control", value = "pos"),
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
    tableOutput(NS(id, "dr_result")),
    plotOutput(NS(id, "dr_result_plot")),
    verbatimTextOutput(NS(id, "dr_error"))
  )
}

DoseResponseServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {

    # Render split by group
    output$open_split_by_group <- renderUI({
      actionButton(NS(id, "open_split_by_group"),
        "Open the split by group functionality",
        title = "Open the split by group helper window",
        disabled = is.null(data$df) || !is.data.frame(data$df)
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
        paste0(
          "The dataset is splitted by the variable ",
          data$filter_col,
          " and the group is ",
          data$filter_group)
      })
    })

    # Remove filter
    observeEvent(input[["remove_filter"]], {
      data$df <- data$backup_df
      data$backup_df <- NULL
      data$filter_col <- NULL
      data$filter_group <- NULL
    })

    observeEvent(input[["df_help_icon"]], {
      showModal(modalDialog(
        title = "Example Dataframe",
         includeHTML("www/df_excerpt_dose_response.html"),
        easyClose = TRUE,
        footer = NULL
      ))
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
      req(!is.null(data$formula))
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
        resPlot <- resP[[1]]
        if (length(resP) >= 2) {
          for (i in seq_along(2:length(resP))) {
            # if (i %% 4 == 0) {
            # resPlot <- resPlot / resP[[i]]
            # } else {
            resPlot <- resPlot + resP[[i]]
            # }
          }
        }
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$dr_error <- renderText(err)
      } else {
        listResults$curr_data <- new("doseResponse", df = resDF, p = resPlot)
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted dose response analysis")
        output$dr_result <- renderTable(resDF, digits = 6)
        output$dr_result_plot <- renderPlot(resPlot)
      }
    }

    observeEvent(input$ic50, {
      drFct()
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
