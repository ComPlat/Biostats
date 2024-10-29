source("check_ast.R")
library(shiny)

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
      fileInput("file1", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,
      .csv"
        )
      ),
      div(
        uiOutput("colnames_list"),
        actionButton("prev_page", "Previous"),
        actionButton("next_page", "Next"),
        class = "boxed-output"
      ),
      br(),
      div(
        h3("Arithmetic Operators"),
        actionButton("add", "+", class = "add-button"),
        actionButton("sub", "-", class = "add-button"),
        actionButton("mul", "*", class = "add-button"),
        actionButton("div", "/", class = "add-button"),
        actionButton("bracket_open", "(", class = "add-button"),
        actionButton("bracket_close", ")", class = "add-button"),
        actionButton("bracket_close", ",", class = "add-button"),
        class = "boxed-output"
      ),
      div(
        h3("Math Functions"),
        actionButton("log", "log", class = "add-button"),
        actionButton("log10", "log10", class = "add-button"),
        actionButton("sqrt", "sqrt", class = "add-button"),
        actionButton("exp", "exp", class = "add-button"),
        actionButton("exponent", "^", class = "add-button"),
        actionButton("sin", "sin", class = "add-button"),
        actionButton("cos", "cos", class = "add-button"),
        actionButton("tan", "tan", class = "add-button"),
        actionButton("sinh", "sinh", class = "add-button"),
        actionButton("cosh", "cosh", class = "add-button"),
        actionButton("tanh", "tanh", class = "add-button"),
        actionButton("asin", "asin", class = "add-button"),
        actionButton("acos", "acos", class = "add-button"),
        actionButton("atan", "atan", class = "add-button"),
        actionButton("abs", "abs", class = "add-button"),
        actionButton("ceil", "ceiling", class = "add-button"),
        actionButton("floor", "floor", class = "add-button"),
        actionButton("trunc", "trunc", class = "add-button"),
        actionButton("round", "round", class = "add-button"),
        class = "boxed-output"
      ),
      div(
        h3("Comparison Operators"),
        actionButton("larger", ">", class = "add-button"),
        actionButton("smaller", "<", class = "add-button"),
        actionButton("larger_eq", ">=", class = "add-button"),
        actionButton("smaller_eq", "<=", class = "add-button"),
        actionButton("eq", "==", class = "add-button"),
        actionButton("not_eq", "!=", class = "add-button"),
        class = "boxed-output"
      ),
      div(
        h3("String Functions"),
        actionButton("grep", "grep", class = "add-button"),
        actionButton("substr", "substr", class = "add-button"),
        actionButton("sub", "sub", class = "add-button"),
        actionButton("paste", "paste", class = "add-button"),
        actionButton("paste0", "paste0", class = "add-button"),
        actionButton("strsplit", "strsplit", class = "add-button"),
        actionButton("tolower", "tolower", class = "add-button"),
        actionButton("toupper", "toupper", class = "add-button"),
        class = "boxed-output"
      ),
      div(
        h3("Statistical & Utils Functions"),
        actionButton("mean", "mean", class = "add-button"),
        actionButton("sd", "standard deviation", class = "add-button"),
        actionButton("median", "median", class = "add-button"),
        actionButton("sum", "sum", class = "add-button"),
        actionButton("min", "min", class = "add-button"),
        actionButton("max", "max", class = "add-button"),
        actionButton("c", "concatenate", class = "add-button"),
        class = "boxed-output"
      ),
      div(
        h3("Random number functions"),
        actionButton("dnorm", "dnorm", class = "add-button"),
        actionButton("pnorm", "pnorm", class = "add-button"),
        actionButton("qnorm", "qnorm", class = "add-button"),
        actionButton("rnorm", "rnorm", class = "add-button"),
        actionButton("dbinom", "dbinom", class = "add-button"),
        actionButton("pbinom", "pbinom", class = "add-button"),
        actionButton("qbinom", "qbinom", class = "add-button"),
        actionButton("rbinom", "rbinom", class = "add-button"),
        actionButton("dpois", "dpois", class = "add-button"),
        actionButton("ppois", "ppois", class = "add-button"),
        actionButton("rpois", "rpois", class = "add-button"),
        actionButton("dunif", "dunif", class = "add-button"),
        actionButton("punif", "punif", class = "add-button"),
        actionButton("qunif", "qunif", class = "add-button"),
        actionButton("runif", "runif", class = "add-button"),
        class = "boxed-output"
      )
    ),
    mainPanel(
      tableOutput("head"),
      div(
        textAreaInput("editable_code", "Operation:", value = "", rows = 12),
        class = "boxed-output"
      ),
      fluidRow(
        column(
          1,
          actionButton("run_op", "Run operation")
        ),
        column(
          4,
          textInput("nc", "New column name:", value = "")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values
  r_vals <- reactiveValues(
    df = NULL,
    current_page = 1, total_pages = 1,
    counter_id = 0
  )


  # Read data
  data <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath, header = TRUE)
  })

  output$head <- renderTable({
    r_vals$df <- data()
    head(r_vals$df, 10)
  })

  # Run operation
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
        type = "error")
      return()
    }
    e <- try({
      ast <- get_ast(op)
      ast <- ast[[length(ast)]]
    })
    if (e == "Error") {
      showNotification("Found unallowed function",
        type = "error")
      return()
    } else if (inherits(e, "try-error")) {
      showNotification(e, type = "error")
      return()
    }
    e <- try({
      new <- with(df, eval(parse(text = code)))
      df[, new_col] <- new
    })
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
      showNotification(err, type = "error")
    }
    # TODO: add check that only column names are used as variables
    # This is only needed for a better user experience
    r_vals$df <- df
    output$head <- renderTable(head(r_vals$df, 10))
    r_vals$counter_id = r_vals$counter_id + 1
  })

  # Create colnames button
  output$colnames_list <- renderUI({
    req(!is.null(r_vals$df))
    req(is.data.frame(r_vals$df))
    colnames <- names(r_vals$df)
    start <- (r_vals$current_page - 1) * 15 + 1
    end <- min(r_vals$current_page * 15, length(colnames))
    button_list <- lapply(colnames[start:end], function(i) {
      actionButton(
        inputId = paste0("colnames_", i, "_", r_vals$counter_id),
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
          current_text <- input$editable_code
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

}

shinyApp(ui, server)
