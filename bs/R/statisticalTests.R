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

testsServer <- function(id, DataModelState, ResultsState) {
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
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        tt <- t_test_V1_2$new(DataModelState$df,DataModelState$formula, input$varEq, input$confLevel, input$altHyp)
        tt$validate()
        tt$eval(ResultsState)
      })
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      } else {
        exportTestValues(
          tests_res = res
        )
      }
    }
    observeEvent(input$tTest, {
      tTest()
    })

    conductTests <- function(method) {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try( {
        st <- statistical_tests_V1_2$new(
          DataModelState$df,DataModelState$formula, input$design, input$pval, input$padj
        )
        st$validate()
        st$eval(ResultsState, method) 
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        err <- paste0(err, "\n", "Test did not run successfully")
        print_err(err)
      } else if (is.null(res)) {
        err <- "Test did not run successfully"
        print_err(err)
      } else {
        exportTestValues(
          tests_res = res
        )
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

}
