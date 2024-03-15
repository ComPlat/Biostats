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

corrServer <- function(id, df, listResults) {
  moduleServer(id, function(input, output, session) {  
      corr_fct <- function(method) {
        req(is.data.frame(df))
        req(input$dep)
        req(input$indep)
        dep <- input$dep
        indep <- input$indep
        d <- df
        fit <- NULL
        err <- NULL
        e <- try(
          fit <- broom::tidy(
            cor.test(d[, dep], d[, indep],
                     method = method,
                     alternative = input$alt,
                     conf.level = input$conflevel))
        )
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
        } else {
          listResults$curr_data <- renderTable(fit, digits = 6)
          listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted test: ", method)
          listResults$all_data[[length(listResults$all_data) + 1]] <- fit
          listResults$all_names <- c(listResults$all_names, 
                                     paste("Test Nr", length(listResults$all_names) + 1, "Conducted test: ", method))
          output$corr_result <- renderTable(fit, digits = 6)
          output$corr_error <- renderText(err)  
        }
      }
      
      observeEvent(input$pear, {
        corr_fct("pearson")
      })
      output$cor_result <- renderTable({
        listResults$data
        }, digits = 6
      )
      
      observeEvent(input$spear, {
        corr_fct("spearman")
      })
      output$cor_result <- renderTable({
        listResults$data
        }, digits = 6
      )
      
      observeEvent(input$kendall, {
        corr_fct("kendall")
      })
      output$cor_result <- renderTable({
        listResults$data
        }, digits = 6
      )
      
      observeEvent(input$corr_save, {
        updateCheckboxGroupInput(session, "TableSaved",
                                 choices = listResults$all_names)
      })
      
      observeEvent(input$download_corr, {
        indices <- which(input$TableSaved == listResults$all_names)
        req(length(indices) >= 1)
        l <- listResults$all_data[indices]
        jsString <- character(length(l))
        for (i in seq_along(l)) {
           if (inherits(l[[i]], "ggplot")) {
             fn <- tempfile(fileext = '.png')
             ggsave(plot = l[[i]], filename = fn)
             jsString[i] <- base64enc::base64encode(fn)
             unlink(fn)
           } else if (inherits(l[[i]], "data.frame")) {
             jsString[i] <- DF2String(l[[i]])
           } else if (is.character(l[[i]])) {
             jsString[i] <- l[[i]]
           }
        }
        session$sendCustomMessage(type = "downloadZip",
                                  list(numberOfResults = length(jsString),
                                       FileContent = jsString))
      })
      
	})
  
  return(listResults)
}





