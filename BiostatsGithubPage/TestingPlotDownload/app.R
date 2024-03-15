library(shiny)
library(ggplot2)
library(shinyjs)
library(base64enc)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
    tags$script(src = "test.js")
  ),
  plotOutput("plot"),
  plotOutput("oldPlot"),
  actionButton("press", "Save current plot"),
  actionButton("pressCreateOldPlot", "Create old plot"),
  actionButton("pressOldPlot", "Save old plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(
    ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
      geom_boxplot()
  )
  
  observeEvent(input$press, {
    session$sendCustomMessage(type = "test",
                              list(ID = "plot"))
  })
  
  p <- reactiveValues(p = NULL)
  observeEvent(input$pressCreateOldPlot, {
    pl <- ggplot(data = CO2, aes(x = Treatment, y = uptake)) +
      geom_boxplot()
    p$p <- pl
    output$oldPlot <- renderPlot(pl)
  })
  
  observeEvent(input$pressOldPlot, {
    req(!is.null(p$p))
    pl <- p$p
    fn <- tempfile(fileext = '.png')
    ggsave(filename = fn, plot = pl, device = "png")
    b64 <- base64enc::base64encode(fn)
    session$sendCustomMessage(type = "testOldPlot",
                              list(Base64String = b64))
    unlink(fn)
  })
}

shinyApp(ui = ui, server = server)
