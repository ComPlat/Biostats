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
        h4(strong("Results of test:")),
        tableOutput(NS(id, "corr_result")),
        verbatimTextOutput(NS(id, "corr_error")),
        actionButton(NS(id, "corr_save"), "Add output to result-file"),
        actionButton(NS(id, "download_corr"), "Save results"),
        checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL),
        
        tags$script(
          "
          Shiny.addCustomMessageHandler('downloadZip', function(message) {
            var files = message.files;
            var filenames = message.filenames;
            var zip = new JSZip();
            for (var i = 0; i < files.length; i++) {
              zip.file(filenames[i], files[i]);
            }
            zip.generateAsync({type:'blob'}).then(function(content) {
              saveAs(content, 'downloaded_files.zip');
            });
          });
          "
        )
  )
}

corrServer <- function(id, df, listResults) {
  moduleServer(id, function(input, output, session) {  
      corr_fct <- function(method) {
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
        # indices <- which(input$TableSaved == listResults$all_names)
        # req(length(indices) >= 1)
        # files <- c()
        # l <- listResults$all_data[indices]
        # for (i in seq_along(l)) {
        #   if (inherits(l[[i]], "ggplot")) {
        #     files <- c(files, tempfile(fileext = ".png"))
        #     ggsave(plot = l[[i]], filename = files[length(files)])
        #   } else {
        #     files <- c(files, tempfile(fileext = ".csv"))
        #     write.csv(l[[i]], files[length(files)], 
        #               quote = FALSE, row.names = FALSE)  
        #   }
        # }
        # zip_file <- tempfile(fileext = ".zip")
        # showNotification(zip_file, duration = 0)
        # zip(zip_file, files) # issue: is this independent of OS?
        # for (i in seq_along(files)) {
        #   if (file.exists(files[i])) unlink(files[i])
        # }
        
        files <- c("File content 1", "File content 2")
        filenames <- c("file1.txt", "file2.txt")
        session$sendCustomMessage(type = "downloadZip", list(files = files, filenames = filenames))
      })
      
	})
  
  return(listResults)
}





