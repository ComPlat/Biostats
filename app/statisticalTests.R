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
      actionButton(NS(id, "aovTest"), "ANOVA",
        title = "Use ANOVA (Analysis of Variance) when comparing the means of more than two groups, assuming the data is normally distributed and variances are equal across groups. For more information see the Assumption tab"
      ),
      actionButton(NS(id, "kruskalTest"), "Kruskal-Wallis Test",
        title = "Use the Kruskal-Wallis test when comparing more than two groups but the assumptions of normality or equal variances are not met. It is a non-parametric test. For more information see the Assumption tab"
      ),
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
      uiOutput(NS(id, "padjUI"))
    )
  )
}

testsUI <- function(id) {
  fluidRow(
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
    )
  )
}

testsServer <- function(id, data, listResults) {
  moduleServer(id, function(input, output, session) {
    # Render p adjustment methods
    output[["padjUI"]] <- renderUI({
      if (input$PostHocTests == "kruskalTest" || input$PostHocTests == "LSD") {
        return(
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
      }
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

    tTest <- function() {
      req(is.data.frame(data$df))
      df <- data$df
      req(!is.null(data$formula))
      formula <- data$formula
      err <- NULL
      fit <- NULL
      e <- try({
        eq <- TRUE
        if (input$varEq == "noeq") {
          eq <- FALSE
        }
        fit <- broom::tidy(t.test(formula,
          data = df, conf.level = input$confLevel,
          alternative = input$altHyp, var.equal = eq
        ))
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_req(FALSE, err)
      } else {
        listResults$counter <- listResults$counter + 1
        new_name <- paste0(
          "TTestNr", listResults$counter
        )
        listResults$all_data[[new_name]] <- fit
        exportTestValues(
          tests_res = fit
        )
      }
    }

    observeEvent(input$tTest, {
      tTest()
    })

    conductTests <- function(method) {
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
        err <- paste0(err, "\n", "Could not use Formula")
        output$test_error <- renderText(err)
      }
      if (is.null(err)) {
        e <- try(
          {
            switch(method,
              aov = {
                fit <- broom::tidy(aov(formula, data = df))
              },
              kruskal = {
                fit <- broom::tidy(kruskal.test(formula, data = df)) # Keep here the restriction for respone ~ predictor
              },
              HSD = {
                check_formula(formula)
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
                check_formula(formula)
                fit <- with(df, kruskal(df[, dep], df[, indep]),
                  alpha = input$pval, p.adj = input$padj, group = TRUE
                )$groups
              },
              LSD = {
                check_formula(formula)
                aov_res <- aov(formula, data = df)
                fit <- agricolae::LSD.test(aov_res,
                  trt = indep,
                  alpha = input$pval, p.adj = input$padj, group = TRUE
                )$groups
              },
              scheffe = {
                check_formula(formula)
                aov_res <- aov(formula, data = df)
                fit <- agricolae::scheffe.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
              },
              REGW = {
                check_formula(formula)
                aov_res <- aov(formula, data = df)
                fit <- agricolae::REGW.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
              }
            )
          },
          silent = TRUE
        )
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          err <- paste0(err, "\n", "Test did not run successfully")
          print_req(FALSE, err)
        } else if (is.null(fit)) {
          err <- paste0(err, "\n", "Test did not run successfully")
          print_req(FALSE, err)
        } else {
          fit <- cbind(fit, row.names(fit))
          names(fit)[ncol(fit)] <- paste0(indep, collapse = ".")
          exportTestValues(
            tests_res = fit
          )
          listResults$counter <- listResults$counter + 1
          new_name <- paste0(
            "Test_", method, "Nr", listResults$counter
          )
          listResults$all_data[[new_name]] <- fit
        }
      }
    }

    observeEvent(input$aovTest, {
      conductTests("aov")
    })

    observeEvent(input$kruskalTest, {
      conductTests("kruskal")
    })

    observeEvent(input$PostHocTest, {
      conductTests(input$PostHocTests)
    })
  })

  return(listResults)
}
