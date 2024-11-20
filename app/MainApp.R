app <- function() {
  ui <- fluidPage(
    # useShinyjs(),
    # includeScript("www/download.js"),
    # tags$head(
    #   tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
    #   tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
    #   tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js")
    # ),
  )

  server <- function(input, output, session) {
  }

  return(list(ui = ui, server = server))
}
