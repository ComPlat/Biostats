# df
# abs_col
# conc_col
# substance_name_col,
# negative_identifier,
# positive_identifier
# path <- system.file("data", package = "MTT")
# df <- read.csv(paste0(path, "/ExampleData.txt"))
# ic50(df, "abs", "conc", "names", "neg", "pos")



DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    textInput(NS(id, "dep"), "dependent Variable", value = "abs"),
    textInput(NS(id, "indep"), "independent Variable", value = "conc"),
    textInput(NS(id, "substanceNames"), "names colum of dependent Variable", value = "names"),
    textInput(NS(id, "negIdentifier"), "identifier for the negative control", value = "neg"),
    textInput(NS(id, "posIdentifier"), "identifier for the positive control", value = "pos"),
    actionButton(NS(id, "ic50"), "conduct analysis")
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
    drFct <- function() {
      output$dr_error <- renderText(NULL)
      req(is.data.frame(data$df))
      df <- data$df
      req(input$dep)
      req(input$indep)
      dep <- input$dep
      indep <- input$indep
      req(input$substanceNames)
      names <- input$substanceNames
      req(input$negIdentifier)
      neg <- input$negIdentifier
      req(input$posIdentifier)
      pos <- input$posIdentifier
      err <- NULL
      resDF <- NULL
      resPlot <- NULL
      e <- try({
        stopifnot(get_ast(str2lang(indep)) != "Error")
        stopifnot(get_ast(str2lang(dep)) != "Error")
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
