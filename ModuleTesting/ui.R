library(shiny)

source("uploadUI.R")
source("meansUI.R")

ui <- fluidPage(
  fileUploadUI("fileUploader"),
  meansUI("means")
)