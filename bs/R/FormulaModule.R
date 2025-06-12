FormulaEditorUI <- function(id) {
  ui <- fluidPage(
    fluidRow(
      uiOutput(NS(id, "model")),
      column(
        width = 6,
        div(
          class = "model",
          h3(class = "title", "Left Side of Statistical Model"),
          uiOutput(NS(id, "colnames_dropdown"))
        ),
        br(),
        br(),
        br(),
        div(
          class = "model",
          actionButton(NS(id, "create_formula"), "Create statistical model", class = "create_button")
        ),
        selectInput(NS(id, "model_type"), "Choose the model type",
          c(
            "Linear" = "Linear",
            "Generalised Linear Model" = "Generalised Linear Model",
            "Optimization Model" = "Optimization Model"
          ),
          selectize = FALSE
        ),
        uiOutput(NS(id, "glm_family_dropdown")),
        uiOutput(NS(id, "glm_link_fct_dropdown")),
        uiOutput(NS(id, "optim_boundaries"))
      ),
      column(
        width = 6,
        div(
          class = "model",
          div(
            style = "position: relative",
            actionButton(
              NS(id, "formula_docu"),
              label = NULL,
              icon = icon("question-circle")
            )
          ),
          uiOutput(NS(id, "colnames_list")),
          br(),
          uiOutput(NS(id, "buttons")),
          div(
            textAreaInput(NS(id, "editable_code"), "Right side of model:", value = "", rows = 12),
            class = "boxed-output"
          )
        )
      )
    )
  )
}

FormulaEditorServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {

    # Create buttons
    output[["buttons"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      button_list <- list(
        actionButton("FO-add", "+",
          class = "add-button",
          title = "Include an additional predictor variable in the model"
        ),
        actionButton("FO-minus", "-",
          class = "add-button",
          title = "Removes an additional predictor variable in the model"
        ),
        actionButton("FO-mul", "*",
          class = "add-button",
          title = "Multiply variables to assess interactions in the model"
        )
      )
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        button_list[[length(button_list) + 1]] <- actionButton("FO-colon", ":",
          class = "add-button",
          title = "Includes the interaction between two variables in the model"
        )
      } else if (input$model_type == "Optimization Model") {
        button_list[[length(button_list) + 1]]  <- actionButton("FO-div", "/",
          class = "add-button",
          title = "Includes nested effects (both variable levels) in the model"
        )
      }
      div(
        h3(class = "title", "Operators"),
        div(
          do.call(tagList, button_list),
          class = "boxed-output"
        )
      )
    })

    # Create colnames button
    output[["colnames_list"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- ""
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        colnames <- names(DataModelState$df)
      } else if (input$model_type == "Optimization Model") {
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
      }
      button_list <- lapply(colnames[1:length(colnames)], function(i) {
        actionButton(
          inputId = paste0("FO-colnames_", i, "_", DataModelState$counter_id),
          label = paste(i),
          class = "add-button",
          title = paste("Select variable", i, "as a predictor for the model")
        )
      })
      div(
        h3(class = "title", "Right Side of Statistical Model"),
        div(
          do.call(tagList, button_list),
          class = "boxed-output"
        )
      )
    })

    # Create colnames dropdown
    output[["colnames_dropdown"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- ""
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        colnames <- names(DataModelState$df)
      } else if (input$model_type == "Optimization Model") {
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
      }
      tooltip <- "Select the dependent variable for your statistical model. This is the outcome you want to predict based on the independent variables."
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("FO-colnames-dropdown_", DataModelState$counter_id),
          label = "Dependent Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    # If glm is choosen create family
    output[["glm_family_dropdown"]] <- renderUI({
      if (input$model_type == "Linear") {
        NULL
      } else if (input$model_type == "Generalised Linear Model") {
        selectInput(inputId = "FO-Family", "The distribution family which describes the residuals",
          c(
            "binomial" = "binomial",
            "gaussian" = "gaussian",
            "Gamma" = "Gamma",
            "inverse.gaussian" = "inverse.gaussian",
            "poisson" = "poisson",
            "quasi" = "quasi",
            "quasibinomial" = "quasibinomial",
            "quasipoisson" = "quasipoisson"
          ),
          selectize = FALSE
        )
      }
    })
    # If glm is choosen create link function
    output[["glm_link_fct_dropdown"]] <- renderUI({
      req(input$Family)
      if (input$model_type == "Linear") {
        NULL
      } else if (input$model_type == "Generalised Linear Model") {
        if (input[["Family"]] == "binomial") {
          selectInput("FO-Link_function", "The link function",
            c(
              "logit" = "logit",
              "probit" = "probit",
              "cauchit" = "cauchit"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] %in% c("gaussian", "Gamma")) {
          selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "inverse" = "inverse"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] == "inverse.gaussian") {
          selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "inverse" = "inverse",
              "1/mu^2" = "1/mu^2"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] == "poisson") {
          selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "sqrt" = "sqrt"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] %in% c("quasi", "quasibinomial", "quasipoisson")) {
          selectInput("FO-Link_function", "The link function", # TODO: requires better description
            c(
              "identity" = "identity",
              "inverse" = "inverse",
              "log" = "log",
              "cloglog" = "cloglog",
              "logit" = "logit",
              "probit" = "probit",
              "1/mu^2" = "1/mu^2",
              "sqrt" = "sqrt"
            ),
            selectize = FALSE
          )
        }
      }
    })
    # Optim UI
    output[["optim_boundaries"]] <- renderUI({
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        NULL
      } else if (input$model_type == "Optimization Model") {
        div(
          numericInput("FO-LowerBoundary", "Lower boundary of parameters", value = 0),
          numericInput("FO-UpperBoundary", "Upper boundary of parameters", value = 100),
          numericInput("FO-Seed", "Seed (start value for random number generation)", value = sample(1:10^6, 1))
        )
      }
    })

    # React to colnames buttons
    observe({
      req(DataModelState$df)
      colnames <- names(DataModelState$df)
      lapply(colnames, function(col) {
        observeEvent(input[[paste0("colnames_", col, "_", DataModelState$counter_id)]], {
          current_text <- input[["editable_code"]]
          updated_text <- paste(current_text, col, sep = " ")
          updateTextAreaInput(session, "editable_code", value = updated_text)
        })
      })
    })

    observeEvent(input$add, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "+", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$mul, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "*", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$minus, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "-", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$colon, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ":", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$div, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "/", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$nested, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "%in%", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$interaction_level, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "^", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$I, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "I(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    # React to create formula
    observeEvent(input$create_formula, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      tryCatch({
        withCallingHandlers(
          expr = {
            response_var <- input[[paste0("colnames-dropdown_", DataModelState$counter_id)]]
            right_site <- input[["editable_code"]]
            cf <- create_formula_V1_2$new(response_var, right_site, DataModelState$df)
            cf$validate()
            model_latex <- NULL
            if (input$model_type == "Linear") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type)
            } else if (input$model_type == "Generalised Linear Model") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type, input$Family, input$`Link_function`)
            } else if (input$model_type == "Optimization Model") {
              model_latex <- cf$eval(
                ResultsState, DataModelState, input$model_type,
                input$LowerBoundary, input$UpperBoundary, input$Seed
              )
            }
            output$model <- renderUI({
              withMathJax(HTML(paste0("$$", model_latex, "$$")))
            })
            pm <- summarise_model_V1_2$new(DataModelState$df, DataModelState$formula)
            pm$validate()
            pm$eval(ResultsState)
          },
          warning = function(warn) {
            print_warn(warn$message)
            invokeRestart("muffleWarning")
          }
        )},
        error = function(err){
          print_err("Invalid formula")
          print_err(err$message)
        }
      )
    })

  })
}
