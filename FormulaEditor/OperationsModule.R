source("check_ast.R")
library(shiny)

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
    sidebarLayout(
      sidebarPanel(

        div(
          uiOutput(NS(id, "colnames_list")),
          actionButton(NS(id, "prev_page"), "Previous"),
          actionButton(NS(id, "next_page"), "Next"),
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
          actionButton(NS(id, "bracket_close"), ",", class = "add-button"),
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
      ),
      mainPanel(
        uiOutput(NS(id, "head")),
        uiOutput(NS(id, "intermediate_results")),
        div(
          textAreaInput(NS(id, "editable_code"), "Operation:", value = "", rows = 12),
          class = "boxed-output"
        ),
        fluidRow(
          column(
            7,
            actionButton(NS(id, "run_op_intermediate"), "Run operation and store intermediate results"),

          ),
          column(
            4,

            textInput(NS(id, "iv"), "Intermediate variable name:", value = "")
          )
        ),
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
      )
    )
  )
}

OperationEditorServer <- function(id, data) {

  moduleServer(id, function(input, output, session) {
    # Reactive values
    r_vals <- reactiveValues(
      df = NULL,
      current_page = 1, total_pages = 1,
      counter_id = 0,
      intermediate_vars = list()
    )

    observe({
      req(is.data.frame(data$df))
      r_vals$df <- data$df
      output$head <- renderUI({
        renderTable(head(r_vals$df))
      })
    })

    # Observe intermeidate results
    output$intermediate_results <- renderUI({
      iv_list <- r_vals$intermediate_vars
      iv_ui <- lapply(names(iv_list), function(name) {
        div(
          h4(name),
          verbatimTextOutput(NS(id, paste0("iv_", name))),
          actionButton(NS(id, paste0("remove_iv_", name)), "Remove", class = "btn-danger")
        )
      })
      do.call(tagList, iv_ui)
    })

    observe({
      iv_list <- r_vals$intermediate_vars
      for (name in names(iv_list)) {
        output[[paste0("iv_", name)]] <- renderPrint({
          iv_list[[name]]
        })
      }
    })

    # Observe and render each intermediate result
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
      df <- r_vals$df
      var_name <- input$iv
      code <- input$editable_code
      op <- try(str2lang(code))
      if (inherits(op, "try-error")) {
        showNotification("Could not convert operation to R code",
          type = "error"
        )
        return()
      }
      e <- try({
        ast <- get_ast(op)
        ast <- ast[[length(ast)]]
      })
      if (e == "Error") {
        showNotification("Found unallowed function",
          type = "error"
        )
        return()
      } else if (inherits(e, "try-error")) {
        showNotification(e, type = "error")
        return()
      }
      e <- try({
        eval_env <- new.env()
        list2env(r_vals$intermediate_vars, envir = eval_env)
        list2env(df, envir = eval_env)
        new <- eval(parse(text = code), envir = eval_env)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        showNotification(err, type = "error")
      }
      # TODO: add check that only column names and ivs are used as variables
      # This is only needed for a better user experience
      # TODO: check that names are valid for variables
      r_vals$intermediate_vars[[var_name]] <- new
    })

    # Run operation and append to df
    observeEvent(input$run_op, {
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      req(input$nc != "")
      df <- r_vals$df
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
        ast <- get_ast(op)
        ast <- ast[[length(ast)]]
      })
      if (e == "Error") {
        showNotification("Found unallowed function",
          type = "error"
        )
        return()
      } else if (inherits(e, "try-error")) {
        showNotification(e, type = "error")
        return()
      }
      e <- try({
        eval_env <- new.env()
        list2env(r_vals$intermediate_vars, envir = eval_env)
        list2env(df, envir = eval_env)
        new <- eval(parse(text = code), envir = eval_env)
        df[, new_col] <- new
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        showNotification(err, type = "error")
      }
      # TODO: add check that only column names are used as variables
      # This is only needed for a better user experience
      r_vals$df <- df
      data$df <- df
      output$head <- renderTable(head(r_vals$df, 10))
      r_vals$counter_id <- r_vals$counter_id + 1
    })

    # Create colnames button
    output[["colnames_list"]] <- renderUI({
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      colnames <- names(r_vals$df)
      start <- (r_vals$current_page - 1) * 15 + 1
      end <- min(r_vals$current_page * 15, length(colnames))
      button_list <- lapply(colnames[start:end], function(i) {
        actionButton(
          inputId = paste0("OP-colnames_", i, "_", r_vals$counter_id),
          label = paste(i),
          class = "add-button"
        )
      })
      do.call(tagList, button_list)
    })

    observeEvent(input$next_page, { # TODO: does not work
      if (r_vals$current_page < r_vals$total_pages) {
        r_vals$current_page <- r_vals$current_page + 1
      }
    })

    observeEvent(input$prev_page, { # TODO: does not work
      if (r_vals$current_page > 1) {
        r_vals$current_page <- r_vals$current_page - 1
      }
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


    #   create_observe_event <- function(session, btn_id, symbol) {
    #   observeEvent(input[[btn_id]], {
    #     current_text <- input$editable_code
    #     updated_text <- paste(current_text, symbol, sep = " ")
    #     updateTextAreaInput(session, "editable_code", value = updated_text)
    #   })
    # }
    #
    # buttons <- list("add" = "+", "sub" = "-", "mul" = "*", "div" = "/", "log" = "log(", ...)
    # lapply(names(buttons), function(btn_id) {
    #   create_observe_event(session, btn_id, buttons[[btn_id]])
    # })

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
