# =============================================================================
# Background Process Lifecycle (using callr::r_process)
# =============================================================================
#
# 1. **Start**
#    - Trigger: User clicks "Add numbers"
#    - Action: Launch `r_process` running in the background with the function `(x + y)`
#              after a delay (`Sys.sleep(5)`).
#
# 2. **Running**
#    - Trigger: Shiny polls every 500ms using `invalidateLater()`.
#    - Action: Check if the process is still alive with `process$is_alive()`.
#
# 3. **Complete**
#    - Trigger: Process exits successfully.
#    - Action: Retrieve result with `process$get_result()`.
#
# 4. **Cancel**
#    - Trigger: User clicks "Cancel".
#    - Action: Send interrupt signal via `process$interrupt()` to stop the process.
#
# 5. **Error Handling**
#    - Trigger: When polling detects the process is dead.
#    - Action: Use `tryCatch(process$get_result())` to handle errors gracefully
#              (including interrupts).
#
# 6. **Cleanup**
#    - Trigger: After process completion or cancellation.
#    - Action: Nullify the process (`process(NULL)`), update status and result
#              in the Shiny UI.
#
# =============================================================================
library(shiny)
library(callr)

# Function to start background process
start_add_process <- function(x, y) {
  callr::r_bg(
    func = function(x, y) {
      Sys.sleep(5)
      x + y
    },
    args = list(x = x, y = y)
  )
}

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

server <- function(input, output, session) {
  output$current_time <- renderText({
    invalidateLater(1000)
    format(Sys.time(), "%H:%M:%S %p")
  })

  process <- reactiveVal(NULL)
  result_val <- reactiveVal(NULL)
  status <- reactiveVal("Idle")

  # Start process
  observeEvent(input$btn, {
    req(is.null(process()) || !process()$is_alive())
    result_val(NULL)
    process(start_add_process(input$x, input$y))
    status("Running...")
  })

  # Cancel process
  observeEvent(input$cancel_btn, {
    req(!is.null(process()))
    req(process()$is_alive())
    process()$interrupt()
    status("Canceled")
  })

  # Poll process status
  observe({
    invalidateLater(500)
    req(!is.null(process()))
    if (!process()$is_alive()) {
      # Safely try fetching the result
      res <- tryCatch(process()$get_result(), error = function(e) e)
      if (inherits(res, "error")) {
        status("Canceled or Failed")
      } else {
        result_val(res)
        status("Completed")
      }
      process(NULL)
    }
  })

  output$status <- renderText({
    status()
  })

  output$sum <- renderText({
    req(!is.null(result_val()))
    paste("Sum:", result_val())
  })
}

shinyApp(ui, server)
