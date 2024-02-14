meansUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tableOutput(ns("means"))
  )
}