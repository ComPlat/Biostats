# TODO: the names of hte columns the new ones have to be modified by makenames

# TODO: store original dataset. Add option to reset dataset 

OperatorEditorSidebar <- function(id) {
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
        "))
    ),

    div(
      uiOutput(NS(id, "colnames_list")),
      class = "boxed-output"
    ),
    br(),
    div(
      h3("Arithmetic Operators"),
      actionButton(NS(id, "add"), "+", class = "add-button"),
      actionButton(NS(id, "sub"), "-", class = "add-button"),
      actionButton(NS(id, "mul"), "*", class = "add-button"),
      actionButton(NS(id, "div"), "/", class = "add-button"),
      actionButton(NS(id, "bracket_open"), "(", class = "add-button"),
      actionButton(NS(id, "bracket_close"), ")", class = "add-button"),
      actionButton(NS(id, "comma"), ",", class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Math Functions"),
      actionButton(NS(id, "log"), "log", class = "add-button"),
      actionButton(NS(id, "log10"), "log10", class = "add-button"),
      actionButton(NS(id, "sqrt"), "sqrt", class = "add-button"),
      actionButton(NS(id, "exp"), "exp", class = "add-button"),
      actionButton(NS(id, "exponent"), "^", class = "add-button"),
      actionButton(NS(id, "sin"), "sin", class = "add-button"),
      actionButton(NS(id, "cos"), "cos", class = "add-button"),
      actionButton(NS(id, "tan"), "tan", class = "add-button"),
      actionButton(NS(id, "sinh"), "sinh", class = "add-button"),
      actionButton(NS(id, "cosh"), "cosh", class = "add-button"),
      actionButton(NS(id, "tanh"), "tanh", class = "add-button"),
      actionButton(NS(id, "asin"), "asin", class = "add-button"),
      actionButton(NS(id, "acos"), "acos", class = "add-button"),
      actionButton(NS(id, "atan"), "atan", class = "add-button"),
      actionButton(NS(id, "abs"), "abs", class = "add-button"),
      actionButton(NS(id, "ceil"), "ceiling", class = "add-button"),
      actionButton(NS(id, "floor"), "floor", class = "add-button"),
      actionButton(NS(id, "trunc"), "trunc", class = "add-button"),
      actionButton(NS(id, "round"), "round", class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Comparison Operators"),
      actionButton(NS(id, "larger"), ">", class = "add-button"),
      actionButton(NS(id, "smaller"), "<", class = "add-button"),
      actionButton(NS(id, "larger_eq"), ">=", class = "add-button"),
      actionButton(NS(id, "smaller_eq"), "<=", class = "add-button"),
      actionButton(NS(id, "eq"), "==", class = "add-button"),
      actionButton(NS(id, "not_eq"), "!=", class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("String Functions"),
      actionButton(NS(id, "grep"), "grep", class = "add-button"),
      actionButton(NS(id, "substr"), "substr", class = "add-button"),
      actionButton(NS(id, "sub"), "sub", class = "add-button"),
      actionButton(NS(id, "paste"), "paste", class = "add-button"),
      actionButton(NS(id, "paste0"), "paste0", class = "add-button"),
      actionButton(NS(id, "strsplit"), "strsplit", class = "add-button"),
      actionButton(NS(id, "tolower"), "tolower", class = "add-button"),
      actionButton(NS(id, "toupper"), "toupper", class = "add-button"),
      actionButton(NS(id, "get_rows"), "get_rows", class = "add-button",
        title = 'Filter by row. For example get_rows(df, ColName == "Control") or get_rows(df, colName == 10)'),
      actionButton(NS(id, "get_cols"), "get_cols", class = "add-button",
        title = 'Extract column from a data frame (a table).
                 For example get_cols(df, ColName) or get_cols(df, ColName1, ColName2)'),
      class = "boxed-output"
    ),
    div(
      h3("Statistical & Utils Functions"),
      actionButton(NS(id, "mean"), "mean", class = "add-button"),
      actionButton(NS(id, "sd"), "standard deviation", class = "add-button"),
      actionButton(NS(id, "median"), "median", class = "add-button"),
      actionButton(NS(id, "sum"), "sum", class = "add-button"),
      actionButton(NS(id, "min"), "min", class = "add-button"),
      actionButton(NS(id, "max"), "max", class = "add-button"),
      actionButton(NS(id, "c"), "concatenate", class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Convert types of columns"),
      actionButton(NS(id, "as.char"), "convert to character",
        title = "Convert a column of the dataset or an intermediate variable to character. For example as.char(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as.int"), "convert to integer",
        title = "Convert a column of the dataset or an intermediate variable to integer. For example as.int(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as.real"), "convert to real number",
        title = "Convert a column of the dataset or an intermediate variable to a real number. For example as.real(ColName)",
        class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Random number functions"),
      actionButton(NS(id, "dnorm"), "dnorm", class = "add-button"),
      actionButton(NS(id, "pnorm"), "pnorm", class = "add-button"),
      actionButton(NS(id, "qnorm"), "qnorm", class = "add-button"),
      actionButton(NS(id, "rnorm"), "rnorm", class = "add-button"),
      actionButton(NS(id, "dbinom"), "dbinom", class = "add-button"),
      actionButton(NS(id, "pbinom"), "pbinom", class = "add-button"),
      actionButton(NS(id, "qbinom"), "qbinom", class = "add-button"),
      actionButton(NS(id, "rbinom"), "rbinom", class = "add-button"),
      actionButton(NS(id, "dpois"), "dpois", class = "add-button"),
      actionButton(NS(id, "ppois"), "ppois", class = "add-button"),
      actionButton(NS(id, "rpois"), "rpois", class = "add-button"),
      actionButton(NS(id, "dunif"), "dunif", class = "add-button"),
      actionButton(NS(id, "punif"), "punif", class = "add-button"),
      actionButton(NS(id, "qunif"), "qunif", class = "add-button"),
      actionButton(NS(id, "runif"), "runif", class = "add-button"),
      class = "boxed-output"
    )
  )
}

OperatorEditorUI <- function(id) {
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        .boxed-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        }
        .var-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        display: inline-block;
        width: auto;
        }
        .var-box-output {
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
        "))
    ),
    div(
      textAreaInput(NS(id, "editable_code"), "Operation:", value = "", rows = 12),
      class = "boxed-output"
    ),
    br(),
    div(
      class = "boxed-output",
      fluidRow(
        column(
          7,
          actionButton(NS(id, "run_op_intermediate"), "Run operation and store intermediate results"),

        ),
        column(
          4,
          textInput(NS(id, "iv"), "Intermediate variable name:", value = "")
        )
      )
    ),
    br(),
    div(
      class = "boxed-output",
      fluidRow(

        column(
          7,
          actionButton(NS(id, "run_op"), "Run operation and append to dataset")
        ),
        column(
          4,
          textInput(NS(id, "nc"), "New column name:", value = "")
        )
      )
    ),
    uiOutput(NS(id, "head")),
    uiOutput(NS(id, "intermediate_results"))
  )
}

OperationEditorServer <- function(id, data) {

  moduleServer(id, function(input, output, session) {
    # Reactive values
    r_vals <- reactiveValues(
      df = NULL, df_name = "df",
      current_page = 1, total_pages = 1,
      counter_id = 0,
      intermediate_vars = list()
    )

    # Data
    observe({
      req(is.data.frame(data$df))
      r_vals$df <- data$df
      r_vals$df_name <- create_df_name(r_vals$df_name, names(data$df))
      r_vals$intermediate_vars[[r_vals$df_name]] <- data$df
      output$head <- renderUI({
        col_info <- sapply(data$df, function(col) class(col)[1]) |>
          t() |>
          as.data.frame()
        names(col_info) <- names(r_vals$df)
        div(
          class = "var-box-output",
          h4("df",
            title =
            "This is the dataset. Using the text df you can access the entire dataset. If you only want to work with one of the column you can use the respective column title. As a side note only the first 6 rows of the data table are shown.",
            class = "var-output"),
          div(
            title = "This displays the current types for each column",
            renderTable({
              col_info
            })
          ),
          renderTable({
            head(r_vals$df)
          })
        )
      })
    })

    # Create colnames button
    output[["colnames_list"]] <- renderUI({
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      r_vals$df_name <- create_df_name(r_vals$df_name, names(data$df))
      colnames <- c(r_vals$df_name, names(r_vals$df))
      button_list <- lapply(colnames[1:length(colnames)], function(i) {
        if (i == r_vals$df_name) {
          return(actionButton(
            inputId = paste0("OP-colnames_", i, "_", r_vals$counter_id),
            label = paste(i),
            title = paste0("Click button if you want to use the entire dataset"),
            class = "add-button"
          ))
        } else {
          return(actionButton(
            inputId = paste0("OP-colnames_", i, "_", r_vals$counter_id),
            label = paste(i),
            title = paste0("Click button if you want to use the column: ", i),
            class = "add-button"
          ))
        }
      })
      do.call(tagList, button_list)
    })

    # React to colnames buttons
    observe({
      req(r_vals$df)
      r_vals$df_name <- create_df_name(r_vals$df_name, names(data$df))
      colnames <- c(r_vals$df_name, names(r_vals$df))
      lapply(colnames, function(col) {
        observeEvent(input[[paste0("colnames_", col, "_", r_vals$counter_id)]], {
          current_text <- input[["editable_code"]]
          updated_text <- paste(current_text, col, sep = " ")
          updateTextAreaInput(session, "editable_code", value = updated_text)
        })
      })
    })

    # Observe intermeidate results
    output$intermediate_results <- renderUI({
      iv_list <- r_vals$intermediate_vars
      if (length(iv_list) == 1) return()
      iv_list <- iv_list[names(iv_list) != r_vals$df_name]
      iv_ui <- lapply(names(iv_list), function(name) {
        div(
          class = "var-box-output",
          h4(name, title = paste0("This is the variable ", name,
            ". You can use it by entering: ", name, " within the Operation text field."), class = "var-output"),
          verbatimTextOutput(NS(id, paste0("iv_", name))),
          actionButton(NS(id, paste0("remove_iv_", name)), "Remove", class = "btn-danger")
        )
      })
      do.call(tagList, iv_ui)
    })

    # Show intermediate variables
    observe({
      iv_list <- r_vals$intermediate_vars
      lapply(names(iv_list), function(name) {
        observeEvent(r_vals$intermediate_vars[[name]], {
          output[[paste0("iv_", name)]] <- renderPrint({
            r_vals$intermediate_vars[[name]]
          })
        }, ignoreInit = TRUE)
      })
    })

    # Observe remove of intermediate variables
    observe({
      iv_list <- r_vals$intermediate_vars
      for (name in names(iv_list)) {
        output[[paste0("iv_", name)]] <- renderPrint({
          iv_list[[name]]
        })
        observeEvent(input[[paste0("remove_iv_", name)]], {
          r_vals$intermediate_vars[[name]] <- NULL
          showNotification(paste("Removed intermediate result:", name), type = "message")
        }, ignoreInit = TRUE)
      }
    })

    # Run operation and store in intermediate result
    observeEvent(input$run_op_intermediate, {
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      req(input$iv != "")
      var_name <- input$iv |> make.names()
      if (var_name %in% names(r_vals$df)) {
        showNotification("Found invalid variable name",
          type = "error"
        )
        return()
      }
      if (var_name == r_vals$df_name) {
        showNotification("Found invalid variable name df. This name is reserved for the dataset",
          type = "error"
        )
        return()
      }
      code <- input$editable_code
      op <- try(str2lang(code))
      if (inherits(op, "try-error")) {
        showNotification("Could not convert operation to R code",
          type = "error"
        )
        return()
      }
      e <- try({
        vars <- c(r_vals$df_name, names(r_vals$df))
        if (length(r_vals$intermediate_vars) >= 1) {
          vars <- c(vars, names(r_vals$intermediate_vars))
        }
        check_ast(op, vars)
      })
      if (inherits(e, "try-error")) {
        showNotification(e, type = "error")
        return()
      }
      e <- try({
        eval_env <- new.env()
        list2env(r_vals$intermediate_vars, envir = eval_env)
        list2env(r_vals$df, envir = eval_env) # NOTE: this adds each column as own variable
        new <- eval(parse(text = code), envir = eval_env)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        showNotification(err, type = "error")
      }
      r_vals$intermediate_vars[[var_name]] <- new
    })

    # Run operation and append to df
    observeEvent(input$run_op, {
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      req(input$nc != "")
      new_col <- input$nc
      code <- input$editable_code
      op <- try(str2lang(code))
      if (inherits(op, "try-error")) {
        showNotification("Could not convert operation to R code",
          type = "error"
        )
        return()
      }
      e <- try({
        vars <- c(r_vals$df_name, names(r_vals$df))
        if (length(r_vals$intermediate_vars) >= 1) {
          vars <- c(vars,names(r_vals$intermediate_vars))
        }
        check_ast(op, vars)
      })
      if (inherits(e, "try-error")) {
        showNotification(e, type = "error")
        return()
      }
      e <- try({
        eval_env <- new.env()
        list2env(r_vals$intermediate_vars, envir = eval_env)
        list2env(r_vals$df, envir = eval_env)  # NOTE: this adds each column as own variable
        new <- eval(parse(text = code), envir = eval_env)
        r_vals$df[, new_col] <- new
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        showNotification(err, type = "error")
      }
      data$df <- r_vals$df
      output$head <- renderTable(head(r_vals$df, 10))
      r_vals$counter_id <- r_vals$counter_id + 1
    })

    observeEvent(input$add, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "+", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$sub, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "-", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$mul, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "*", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$div, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "/", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$bracket_open, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$bracket_close, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ")", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$comma, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ",", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$log, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "log(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$log10, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "log10(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sqrt, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sqrt(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$exp, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "exp(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$exponent, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "^(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sin, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sin(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$cos, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "cos(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$tan, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tan(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sinh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sinh(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$cosh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "cosh(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$tanh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tanh(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$asin, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "asin(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$acos, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "acos(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$atan, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "atan(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$abs, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "abs(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$ceil, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "ceil(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$floor, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "floor(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$trunc, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "trunc(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$round, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "round(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$larger, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ">", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$smaller, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "<", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$larger_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ">=", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$smaller_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "<=", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "==", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$not_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "!=", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$grep, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "grep(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$substr, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "substr(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sub, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sub(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$paste, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "paste(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$paste0, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "paste0(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$strsplit, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "strsplit(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$tolower, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tolower(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$toupper, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "toupper(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$get_rows, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_rows(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$get_cols, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_cols(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$mean, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "mean(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sd, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sd(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$median, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "median(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sum, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sum(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$min, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "min(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$max, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "max(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$c, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "c(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as.char, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.char(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as.int, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.int(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as.real, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.real(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "dnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$pnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "pnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$qnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "qnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$rnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "rnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "dbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$pbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "pbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$qbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "qbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$rbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "rbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dpois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "dpois(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$ppois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "ppois(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$rpois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "rpois(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dunif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "dunif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$punif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "punif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$qunif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "qunif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$runif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "runif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

  })
} 
