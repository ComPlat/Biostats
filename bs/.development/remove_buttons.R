library(shiny)

ui <- fluidPage(
  titlePanel("Dynamic List with Remove Buttons"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_input", "Enter a number:", value = 0),
      actionButton("add_btn", "Add to List")
    ),
    mainPanel(
      uiOutput("dynamic_list")
    )
  )
)

server <- function(input, output, session) {
  listValues <- reactiveVal(list())
  observeEvent(input$add_btn, {
    current_list <- listValues()
    new_item_name <- paste0("item_", length(current_list) + 1)
    current_list[[new_item_name]] <- input$num_input
    listValues(current_list)
  })

  # Dynamically render the list UI
  output$dynamic_list <- renderUI({
    current_list <- listValues()
    if (length(current_list) == 0) {
      return("No items in the list.")
    }
    # Create UI elements for each item in the list
    tagList(lapply(names(current_list), function(name) {
      div(
        style = "margin-bottom: 10px;",
        span(paste(name, ":", current_list[[name]]), style = "margin-right: 10px;"),
        actionButton(name, "Remove", class = "btn-danger btn-sm")
      )
    }))
  })

  # Observe and handle remove buttons dynamically
  observe({
    current_list <- listValues()
    lapply(names(current_list), function(name) {
      observeEvent(input[[name]], {
        current_list <- listValues()
        current_list[[name]] <- NULL  # Remove the item
        listValues(current_list)
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui, server)

