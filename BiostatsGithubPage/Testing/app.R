library(shiny)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Custom Message Handler Demo with Download"),
  mainPanel(
    textInput("user_message", "Enter your message:"),
    actionButton("trigger_button", "Trigger Custom Message Handler"),
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "updateField.js"),
      tags$script(src = "downloadText.js"),
      tags$script(src = "downloadZip.js"),
      tags$script(src = "downloadPlot.js")
    ),
    div(id = "output", style = "margin-top: 10px;"),
    br(),
    
    plotOutput("plotRes"),
    
    actionButton("download_button", "Download Appended Messages"),
    actionButton("download_zip", "Download Appended Messages"),
    actionButton("download_plot", "Download Appended Messages")
  )
)

server <- function(input, output, session) {
  observeEvent(input$trigger_button, {
    user_message <- input$user_message
    session$sendCustomMessage("updateField", list(message = user_message))
  })
  
  observeEvent(input$download_button, {
    session$sendCustomMessage(type = "downloadText", list(text = "test", file = "bla.csv"))
  })
  
  observeEvent(input$download_zip, {
    files <- c("File content 1", "File content 2")
    filenames <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
    session$sendCustomMessage(type = "downloadZip", list(files = files, filenames = filenames))
  })
  
  observeEvent(input$download_plot, {
    session$sendCustomMessage(type = "downloadText",
                              list(ID = "plotRes"))
  })
  
  output$plotRes <- renderPlot(
    ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point()
  )
  
}

options(passUrlParams = TRUE)
shiny::shinyApp(ui = ui, server = server, options = list(passUrlParams = TRUE))
