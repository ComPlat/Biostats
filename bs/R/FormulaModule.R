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
        )
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
          h3(class = "title", "Right Side of Statistical Model"),
          div(
            uiOutput(NS(id, "colnames_list")),
            class = "boxed-output"
          ),
          br(),
          div(
            h3("Arithmetic Operators"),
            actionButton(NS(id, "add"), "+",
              class = "add-button",
              title = "Include an additional predictor variable in the model"
            ),
            actionButton(NS(id, "minus"), "-",
              class = "add-button",
              title = "Removes an additional predictor variable in the model"
            ),
            actionButton(NS(id, "mul"), "*",
              class = "add-button",
              title = "Multiply variables to assess interactions in the model"
            ),
            actionButton(NS(id, "colon"), ":",
              class = "add-button",
              title = "Includes the interaction between two variables in the model"
            ),
            actionButton(NS(id, "div"), "/",
              class = "add-button",
              title = "Includes nested effects (both variable levels) in the model"
            ),
            actionButton(NS(id, "nested"), "%in%",
              class = "add-button",
              title = "Includes nested effects (both variable levels) without including the main level"
            ),
            actionButton(NS(id, "interaction_level"), "interaction_level",
              class = "add-button",
              title = "Specifies the interaction level in the model"
            ),
            actionButton(NS(id, "I"), "Add arithmetic operations in the model",
              class = "add-button",
              title = "Specifies arithmetic operations within I() which are interpreted as normal arithmetic operations"
            ),
            class = "boxed-output"
          ),
          div(
            textAreaInput(NS(id, "editable_code"), "Right side of model:", value = "", rows = 12),
            class = "boxed-output"
          )
        )
      )
    )
  )
}

FormulaEditorServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive values
    r_vals <- reactiveValues(
      df = NULL,
      counter_id = 0
    )

    observe({
      req(is.data.frame(data$df))
      r_vals$df <- data$df
      output$head <- renderTable({
        head(r_vals$df)
      })
    })

    # Create colnames button
    output[["colnames_list"]] <- renderUI({
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      colnames <- names(r_vals$df)
      button_list <- lapply(colnames[1:length(colnames)], function(i) {
        actionButton(
          inputId = paste0("FO-colnames_", i, "_", r_vals$counter_id),
          label = paste(i),
          class = "add-button",
          title = paste("Select variable", i, "as a predictor for the model")
        )
      })
      do.call(tagList, button_list)
    })

    # Create colnames dropdown
    output[["colnames_dropdown"]] <- renderUI({
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      colnames <- names(r_vals$df)
      tooltip <- "Select the dependent variable for your statistical model. This is the outcome you want to predict based on the independent variables."

      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("FO-colnames-dropdown_", r_vals$counter_id),
          label = "Dependent Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    # React to colnames buttons
    observe({
      req(r_vals$df)
      colnames <- names(r_vals$df)
      lapply(colnames, function(col) {
        observeEvent(input[[paste0("colnames_", col, "_", r_vals$counter_id)]], {
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
      print_req(is.data.frame(r_vals$df), "The dataset is missing")
      tryCatch({
        withCallingHandlers(
          expr = {
            selected_col <- input[[paste0("colnames-dropdown_", r_vals$counter_id)]]
            current_text <- input[["editable_code"]]
            formula <- paste(selected_col, " ~ ", current_text)
            formula <- as.formula(formula)
            check_ast(formula, colnames(r_vals$df))
            data$formula <- formula
            model <- lm(formula, data = r_vals$df)
            model_latex <- extract_eq(model, wrap = TRUE)
            output$model <- renderUI({
              withMathJax(HTML(paste0("$$", model_latex, "$$")))
            })
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
