library(shiny)
library(bslib)
library(future)
library(promises)
future::plan(multisession)

ui <- page_fluid(
  p("The time is ", textOutput("current_time", inline=TRUE)),
  hr(),
  numericInput("x", "x", value = 1),
  numericInput("y", "y", value = 2),
  input_task_button("btn", "Add numbers"),
  textOutput("sum")
)

server <- function(input, output, session) {
  output$current_time <- renderText({
    invalidateLater(1000)
    format(Sys.time(), "%H:%M:%S %p")
  })

  sum_values <- ExtendedTask$new(function(x, y) {
    future_promise({
      Sys.sleep(5)
      x + y
    })
  }) |> bind_task_button("btn")

  observeEvent(input$btn, {
    sum_values$invoke(input$x, input$y)
  })

  output$sum <- renderText({
    sum_values$result()
  })
}

shinyApp(ui, server)
