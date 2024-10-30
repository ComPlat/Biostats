FormulaEditorUI <- function(id) {
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        .boxed-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        }
        .add-button {
        position: relative;
        padding-right: 20px;
        }
        .add-button::after {
        content: '\\2295';
        position: absolute;
        top: 1.1px;
        right: 5px;
        font-size: 16px;
        font-weight: bold;
        color: #900C3F;
        background-color: white;
        width: 15px;
        height: 15px;
        display: flex;
        justify-content: center;
        align-items: center;
        }
        .model {
        background-color: #f8f9fa;
        padding: 15px;
        border: 2px solid #c8c8c8;
        border-radius: 5px;
        margin-top: 10px;
        }
        .title {
        font-size: 14px;
        font-weight: bold;
        margin-bottom: 10px;
        color: #333;
        }
        "))
    ),
    fluidRow(
      uiOutput(NS(id, "model")),
      column(
        width = 6,
        div(
          class = "model",
          h3(class = "title", "Left Side of Statistical Model"),
          uiOutput(NS(id, "colnames_dropdown")),
          actionButton(NS(id, "create_formula"), "Create statistical model")
        )
      ),
      column(
        width = 6,
        div(
          class = "model",
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
            actionButton(NS(id, "mul"), "*",
              class = "add-button",
              title = "Multiply variables to assess interactions in the model"
            ),
            # TODO: add Error and / as seperate function
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

    # React to create formula
    observeEvent(input$create_formula, {
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      e <- try({
        selected_col <- input[[paste0("colnames-dropdown_", r_vals$counter_id)]]
        current_text <- input[["editable_code"]]
        formula <- paste(selected_col, " ~ ", current_text)
        formula <- as.formula(formula)
        data$formula <- formula
        model <- lm(formula, data = r_vals$df)
        model_latex <- equatiomatic::extract_eq(model, wrap = TRUE) # TODO: add equatiomatic to DESCRIPTION
        output$model <- renderUI({
          withMathJax(HTML(paste0("$$", model_latex, "$$")))
        })
      })
      if (inherits(e, "try-error")) {
        showNotification("Invalid formula", type = "error")
      }
    })
  })
}
