library(shiny)
library(callr)

# Define the BgProcess class
BgProcess <- R6::R6Class("BgProcess",
  private = list(
    process = NULL,
    result_val = NULL,
    status_val = NULL,
    poll_interval = NULL
  ),
  public = list(
    initialize = function(poll_interval = 500) {
      private$poll_interval <- poll_interval
      private$process <- reactiveVal(NULL)
      private$result_val <- reactiveVal(NULL)
      private$status_val <- reactiveVal("Idle")

      # Polling loop
      observe({
        invalidateLater(private$poll_interval)
        p <- private$process()
        if (!is.null(p) && !p$is_alive()) {
          res <- tryCatch(p$get_result(), error = function(e) e)
          if (inherits(res, "error")) {
            private$status_val("Canceled or Failed")
          } else {
            private$result_val(res)
            private$status_val("Completed")
          }
          private$process(NULL)
        }
      })
    },
    start = function(fun, args = list()) {
      req(is.null(private$process()) || !private$process()$is_alive())
      r6_args <- names(args)[sapply(args, function(x) inherits(x, "R6"))]
      req(length(r6_args) == 0, paste("Cannot pass R6 objects to background process:", paste(r6_args, collapse = ", ")))
      private$result_val(NULL)
      private$process(callr::r_bg(fun, args = args))
      private$status_val("Running...")
    },
    cancel = function() {
      p <- private$process()
      req(!is.null(p), p$is_alive())
      p$interrupt()
      private$status_val("Canceled")
    },
    status = function() private$status_val(),
    result = function() private$result_val()
  )
)

# UI
ui <- fluidPage(
  p("The time is ", textOutput("current_time", inline=TRUE)),
  hr(),
  numericInput("x", "x", value = 1),
  numericInput("y", "y", value = 2),
  actionButton("btn", "Add numbers"),
  actionButton("cancel_btn", "Cancel"),
  textOutput("status"),
  textOutput("sum")
)

# Server
server <- function(input, output, session) {
  output$current_time <- renderText({
    invalidateLater(1000)
    format(Sys.time(), "%H:%M:%S %p")
  })

  # Initialize the background process handler
  bg_task <- BgProcess$new(poll_interval = 500)

  # Start process
  observeEvent(input$btn, {
    bg_task$start(
      fun = function(x, y) { Sys.sleep(5); x + y },
      args = list(x = input$x, y = input$y)
    )
  })

  # Cancel process
  observeEvent(input$cancel_btn, {
    bg_task$cancel()
  })

  # Status output
  output$status <- renderText({ bg_task$status() })

  # Result output
  output$sum <- renderText({
    req(bg_task$status() == "Completed")
    paste("Sum:", bg_task$result())
  })
}

shinyApp(ui, server)
