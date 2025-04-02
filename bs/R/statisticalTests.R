testsSidebarUI <- function(id) {
  tabPanel(
    "Tests",
    br(),
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

    tTest <- function() {
      print_req(is.data.frame(data$df), "The dataset is missing")
      print_form(data$formula)
      df <- data$df
      formula <- data$formula
      fit <- NULL
      e <- try({
        withCallingHandlers(
          {
            eq <- TRUE
            if (input$varEq == "noeq") {
              eq <- FALSE
            }
            fit <- broom::tidy(t.test(formula,
              data = df, conf.level = input$confLevel,
              alternative = input$altHyp, var.equal = eq
            ))
            check_rls(listResults$all_data, fit)
            fit
          },
          warning = function(warn) {
            print_warn(warn$message)
            invokeRestart("muffleWarning")
          }
        )
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_err(err)
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
      listResults$history[[length(listResults$history) + 1]] <- list(
        type = "TTest",
        formula = deparse(data$formula),
        conf.level = input$confLevel,
        alternative = input$altHyp,
        var.equal = input$varEq,
        "Result name" = new_name
      )
    }
    observeEvent(input$tTest, {
      tTest()
    })

    conductTests <- function(method) {
      print_req(is.data.frame(data$df), "The dataset is missing")
      print_form(data$formula)
      df <- data$df
      formula <- data$formula
      err <- NULL
      fit <- NULL
      indep <- NULL
      dep <- NULL
      history_data <- NULL
      e <- try({
        indep <- as.character(formula)[3]
        dep <- as.character(formula)[2]
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        err <- paste0(err, "\n", "Could not use Formula")
        print_err(err)
      }
      if (is.null(err)) {
        e <- try(
          {
            withCallingHandlers(
              {
                switch(method,
                  aov = {
                    fit <- broom::tidy(aov(
                      formula,
                      data = df
                    ))
                    history_data <- list(type = "ANOVA", formula = deparse(formula))
                  },
                  kruskal = {
                    fit <- broom::tidy(
                      kruskal.test(formula, data = df)
                    ) # Keep here the restriction for respone ~ predictor
                    history_data <- list(type = "Kruskal-Wallis Test", formula = deparse(formula))
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
                    history_data <- list(type = "Tukey HSD",
                      formula = deparse(formula), Balanced = bal,
                      Pval = input$pval)
                  },
                  kruskalTest = {
                    check_formula(formula)
                    fit <- with(df, kruskal(df[, dep], df[, indep]),
                      alpha = input$pval, p.adj = input$padj, group = TRUE
                    )$groups
                    history_data <- list(type = "Kruskal Wallis post hoc test",
                      formula = deparse(formula),
                      "Adjusted p value method" = input$padj,
                      Pval = input$pval)
                  },
                  LSD = {
                    check_formula(formula)
                    aov_res <- aov(formula, data = df)
                    fit <- agricolae::LSD.test(aov_res,
                      trt = indep,
                      alpha = input$pval, p.adj = input$padj, group = TRUE
                    )$groups
                    history_data <- list(type = "Least significant difference test",
                      formula = deparse(formula),
                      "Adjusted p value method" = input$padj,
                      Pval = input$pval)
                  },
                  scheffe = {
                    check_formula(formula)
                    aov_res <- aov(formula, data = df)
                    fit <- agricolae::scheffe.test(
                      aov_res,
                      trt = indep, alpha = input$pval, group = TRUE
                    )$groups
                    history_data <- list(type = "Scheffe post hoc test",
                      formula = deparse(formula),
                      Pval = input$pval)
                  },
                  REGW = {
                    check_formula(formula)
                    aov_res <- aov(formula, data = df)
                    fit <- agricolae::REGW.test(
                      aov_res,
                      trt = indep, alpha = input$pval, group = TRUE
                    )$groups
                    history_data <- list(type = "REGW post hoc test",
                      formula = deparse(formula),
                      Pval = input$pval)
                  }
                )
                check_rls(listResults$all_data, fit)
                fit
              },
              warning = function(warn) {
                print_warn(warn$message)
                invokeRestart("muffleWarning")
              }
            )
          },
          silent = TRUE
        )
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          err <- paste0(err, "\n", "Test did not run successfully")
          print_err(err)
        } else if (is.null(fit)) {
          err <- paste0(err, "\n", "Test did not run successfully")
          print_err(err)
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
          listResults$history[[length(listResults$history) + 1]] <- c(
            history_data,
            "Result name" = new_name
          )
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
