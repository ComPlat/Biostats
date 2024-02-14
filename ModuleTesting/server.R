library(shiny)

server <- function(input, output, session) {
  source("upload.R")
  source("means.R")
  d <- reactiveValues(data = NULL)
  fileUploadModuleServer("fileUploader", d)
  calcMeans("means", d)
}