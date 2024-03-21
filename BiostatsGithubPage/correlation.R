corrSidebarUI <- function(id) {
    tabPanel(
      "Correlation",
      textInput(NS(id, "dep"), "dependent Variable", value = "var1"),
      textInput(NS(id, "indep"), "independent Variable", value = "var2"),
      actionButton(NS(id, "pear"), "Pearson correlation"),
      actionButton(NS(id, "spear"), "Spearman correlation"),
      actionButton(NS(id, "kendall"), "Kendall correlation"),
      sliderInput(NS(id, "conflevel"), "Confidence level of the interval",
                  min = 0, max = 1, value = 0.95),
      selectInput(NS(id, "alt"), "Alternative hypothesis",
                  c("Two sided" = "two.sided",
                    "Less" = "less",
                    "Greater" = "greater"))
    )
}

corrUI <- function(id) {
  fluidRow(
        tags$head(
          tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
          tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
          tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
          tags$script(src = "download.js")
        ),
        h4(strong("Results of test:")),
        tableOutput(NS(id, "corr_result")),
        verbatimTextOutput(NS(id, "corr_error")),
        actionButton(NS(id, "corr_save"), "Add output to result-file"),
        actionButton(NS(id, "download_corr"), "Save results"),
        checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL)
  )
}

corrServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {  
      corr_fct <- function(method) {
        output$corr_error <- renderText(NULL)    
        req(is.data.frame(data$df))
        df <- data$df
        req(input$dep)
        req(input$indep)
        dep <- input$dep
        indep <- input$indep
        d <- df
        fit <- NULL
        err <- NULL
        e <- try({
          stopifnot(get_ast(str2lang(indep)) != "Error")
          stopifnot(get_ast(str2lang(dep)) != "Error")
          fit <- broom::tidy(
            cor.test(d[, dep], d[, indep],
                     method = method,
                     alternative = input$alt,
                     conf.level = input$conflevel))
        })
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          output$corr_error <- renderText(err)
        } else {
          listResults$curr_data <- fit
          listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted test: ", method)
          output$corr_result <- renderTable(fit, digits = 6)  
        }
      }
      
      observeEvent(input$pear, {
        corr_fct("pearson")
      })
      output$cor_result <- renderTable({
        listResults$curr_data
        }, digits = 6
      )
      
      observeEvent(input$spear, {
        corr_fct("spearman")
      })
      output$cor_result <- renderTable({
        listResults$curr_data
        }, digits = 6
      )
      
      observeEvent(input$kendall, {
        corr_fct("kendall")
      })
      output$cor_result <- renderTable({ # issue: check whether this is required
        listResults$curr_data
        }, digits = 6
      )
      
      observeEvent(input$corr_save, {
        if(is.null(listResults$curr_name)) return(NULL)
        if (!(listResults$curr_name %in% unlist(listResults$all_names)) ) {
          listResults$all_data[[length(listResults$all_data) + 1]] <- listResults$curr_data
          listResults$all_names[[length(listResults$all_names) + 1]] <- listResults$curr_name  
        }
        updateCheckboxGroupInput(session, "TableSaved",
                                 choices = listResults$all_names)
      })
      
      observeEvent(input$download_corr, {
        lr <- unlist(listResults$all_names)
        indices <- sapply(input$TableSaved, function(x) {
          which(x == lr)
        })
        req(length(indices) >= 1)
        l <- listResults$all_data[indices]
        jsString <- createJSString(l)
        session$sendCustomMessage(type = "downloadZip",
          list(numberOfResults = length(jsString),
           FileContent = jsString))
      })
      
	})
  
  return(listResults)
}





