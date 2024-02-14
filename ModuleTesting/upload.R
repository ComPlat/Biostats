fileUploadModuleServer <- function(id, sharedData) {
  moduleServer(
    id,
    function(input, output, session) {
      uploaded_file <- reactive({
        req(input$fileInput)
        read.csv(input$fileInput$datapath)
      })
      
      observe({
        sharedData$data <- uploaded_file()  
      })
      
      output$fileData <- renderTable({
        uploaded_file()
      })
    }
  )
}
