source("formula_editor.R")

ui <- fluidPage(
  titlePanel("Pop-Up Example in Shiny"),
  sidebarLayout(
    sidebarPanel(
      # Button to trigger the pop-up
      actionButton("show_popup", "Show Pop-Up")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
