testsSidebarUI <- function(id) {
  tabPanel(
    "Tests",
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
    conditionalPanel(
      condition = "input.TestsConditionedPanels == 'Two groups'",
      sliderInput(NS(id, "confLevel"), "Confidence level of the interval",
        min = 0, max = 1, value = 0.95
      ),
      selectInput(
        NS(id, "altHyp"), "Alternative hypothesis",
        c(
          "Two sided" = "two.sided",
          "Less" = "less",
          "Greater" = "greater"
        )
      ),
      selectInput(
        NS(id, "paired"), "Paired or unpaired t-test",
        c(
          "Unpaired" = "up",
          "Paired" = "p"
        )
      ),
      selectInput(
        NS(id, "varEq"), "Are the two variances treated as equal or not?",
        c(
          "Equal" = "eq",
          "Not equal" = "noeq"
        )
      ),
      actionButton(NS(id, "tTest"), "t test")
    ),
    conditionalPanel(
      condition = "input.TestsConditionedPanels == 'More than two groups'",
      actionButton(NS(id, "aovTest"), "anova"),
      actionButton(NS(id, "kruskalTest"), "kruskal wallis test"),
    ),
    conditionalPanel(
      selectInput(NS(id, "PostHocTests"), "Choose a Post Hoc test",
        choices = c(
          "Tukey HSD" = "HSD", "Kruskal Wallis post hoc test" = "kruskalTest",
          "Least significant difference test" = "LSD",
          "Scheffe post hoc test" = "scheffe", "REGW post hoc test" = "REGW"
        )
      ),
      condition = "input.TestsConditionedPanels == 'Posthoc tests'",
      actionButton(NS(id, "PostHocTest"), "run test"),
      sliderInput(NS(id, "pval"), "P-value",
        min = 0, max = 0.15, value = 0.05
      ),
      selectInput(
        NS(id, "design"), "Design",
        c(
          "Balanced" = "ba",
          "Unbalanced" = "ub"
        )
      ),
      conditionalPanel(
        condition = "input.PostHocTests == 'kruskalPHTest' || input.PostHocTests == 'lsdTest'",
        selectInput(NS(id, "padj"), "Adjusted p method",
          c(
            "Holm" = "holm",
            "Hommel" = "hommel",
            "Hochberg" = "hochberg",
            "Bonferroni" = "bonferroni",
            "BH" = "BH",
            "BY" = "BY",
            "fdr" = "fdr"
          ),
          selectize = FALSE
        )
      )
    )
  )
}

testsUI <- function(id) {
  fluidRow(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "download.js")
    ),
    tabsetPanel(
      tabPanel(
        "Two groups",
        br(),
      ),
      tabPanel(
        "More than two groups",
        br(),
      ),
      tabPanel(
        "Posthoc tests",
        br(),
      ),
      id = "TestsConditionedPanels"
    ),
    h4(strong("Results of test:")),
    tableOutput(NS(id, "test_result")),
    verbatimTextOutput(NS(id, "test_error")),
    actionButton(NS(id, "test_save"), "Add output to result-file"),
    actionButton(NS(id, "download_test"), "Save results"),
    checkboxGroupInput(NS(id, "TableSaved"), "Saved results to file", NULL)
  )
}

testsServer <- function(id, data, listResults) {
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

    tTest <- function() {
      output$test_error <- renderText(NULL)
      req(is.data.frame(data$df))
      df <- data$df
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      fit <- NULL
      e <- try({
        paired <- FALSE
        if (input$paired == "p") {
          paired <- TRUE
        }
        eq <- TRUE
        if (input$varEq == "noeq") {
          eq <- FALSE
        }
        fit <- broom::tidy(t.test(formula,
          data = df, conf.level = input$confLevel,
          alternative = input$alt, paired = paired, var.equal = eq
        ))
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$test_error <- renderText(err)
      } else {
        listResults$curr_data <- fit
        listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted t-test")
        output$test_result <- renderTable(fit, digits = 6)
      }
    }

    observeEvent(input$tTest, {
      tTest()
    })

    conductTests <- function(method) {
      output$test_error <- renderText(NULL)
      req(is.data.frame(data$df))
      df <- data$df
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      fit <- NULL
      indep <- NULL
      dep <- NULL
      e <- try({
        indep <- as.character(formula)[3]
        dep <- as.character(formula)[2]
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        output$test_error <- renderText(err)
      }
      if (is.null(err)) {
        e <- try({
          switch(method,
            aov = {
              fit <- broom::tidy(aov(formula, data = df))
            },
            kruskal = {
              fit <- broom::tidy(kruskal.test(formula, data = df))
            },
            HSD = {
              aov_res <- aov(formula, data = df)
              bal <- input$design
              req(bal)
              if (bal == "Balanced") {
                bal <- TRUE
              } else {
                bal <- FALSE
              }
              fit <- agricolae::HSD.test(aov_res,
                trt = indep,
                alpha = input$pval, group = TRUE, unbalanced = bal
              )$groups
            },
            kruskalTest = {
              fit <- with(df, kruskal(df[, dep], df[, indep]),
                alpha = input$pval, p.adj = input$padj, group = TRUE
              )$groups
            },
            LSD = {
              aov_res <- aov(formula, data = df)
              fit <- agricolae::LSD.test(aov_res,
                trt = indep,
                alpha = input$pval, p.adj = input$padj, group = TRUE
              )$groups
            },
            scheffe = {
              aov_res <- aov(formula, data = df)
              fit <- agricolae::scheffe.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
            },
            REGW = {
              aov_res <- aov(formula, data = df)
              fit <- agricolae::REGW.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
            }
          )
        })
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          output$test_error <- renderText(err)
        } else if (is.null(fit)) {
          output$test_error <- renderText("Result is NULL")
        } else {
          fit <- cbind(fit, row.names(fit))
          names(fit)[ncol(fit)] <- paste0(indep, collapse = ".")
          listResults$curr_data <- fit
          listResults$curr_name <- paste("Test Nr", length(listResults$all_names) + 1, "Conducted: ", method)
          output$test_result <- renderTable(fit, digits = 6)
        }
      }
    }

    observeEvent(input$aovTest, {
      conductTests("aov")
    })

    observeEvent(input$kruskalTest, {
      conductTests("kruskal")
    })

    observeEvent(input$kruskalTest, {
      conductTests("kruskal")
    })

    observeEvent(input$PostHocTest, {
      conductTests(input$PostHocTests)
    })

    observeEvent(input$test_save, {
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

    observeEvent(input$download_test, {
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
