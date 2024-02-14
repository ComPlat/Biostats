fileUploadUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fileInput(ns("fileInput"), "Choose a CSV file"),
    tableOutput(ns("fileData"))
  )
}