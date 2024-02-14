options(shiny.port = 3839)
library(shiny)
library(COMELN)
library(httr)
library(jose)
library(openssl)

ui <- fluidPage(
  fluidRow(
    column(12,
           tableOutput('table')
    )
  )
)

server <- function(input, output, session) {
  output$table <- renderTable({
    file <- COMELN::download(session, "http://localhost:3000", "/home/konrad/Documents/GitHub/shinychem_new/MOCK")
    df <- read.csv(file, header = TRUE, sep = ",")
    data <- summary(df) |> as.data.frame()
    write.csv(data, 
              file = "/home/konrad/Documents/GitHub/shinychem_new/MOCK/test.txt",
              sep = " ", row.names = FALSE, quote = FALSE)
    COMELN::upload(session, "http://localhost:3000", 
                   "/home/konrad/Documents/GitHub/shinychem_new/MOCK/test.txt",
                   new_name = "bla.xlsx")
    return(df)
  })
}
shinyApp(ui, server)