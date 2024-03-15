library(shiny)

ui <- fluidPage(
  mainPanel(
    plotOutput("myplot", click = "plot_click"),
    uiOutput("downloadLink")
  )
)

server <- function(input, output, session) {
  output$myplot <- renderPlot({
    plot(1:10)
  })
  
  output$downloadLink <- renderUI({
    # Generate a data URL for the plot image
    plotDataURL <- reactive({
      plotDataURL <- NULL
      capture.output(print(output$myplot), file = "temp.png", type = "png")
      img <- readBin("temp.png", "raw", file.info("temp.png")$size)
      plotDataURL <- paste("data:image/png;base64,", tools::base64encode(img))
      plotDataURL
    })
    
    # Create a download link
    downloadLink <- tags$a(
      href = plotDataURL(),
      download = "plot.png",
      "Download Plot"
    )
    
    downloadLink
  })
}

shinyApp(ui, server)