# TODO: deploy app to GitHub.io
# All those copying etc. should be done within the github workflow
# Find a solution to show the static html files of the documentation
# automatically copy the www folder to the folder app
# automatically copy the system.files to the folder app
# automatically parse the upload filed as the environment variable is ignored?
files <- list.files("/home/konrad/Documents/Biostats/bs/R/", pattern = ".R", full.names = TRUE)
file.copy(files, "/home/konrad/Documents/Biostats/app/", overwrite = TRUE)
# Replace the upload field
# Copy folder www
# replace system.files with www folder paths

file.create("/home/konrad/Documents/Biostats/app/app.R", overwrite = TRUE)
con <- file("/home/konrad/Documents/Biostats/app/app.R")
code <- function() {
  library(shiny)
  library(shinyWidgets)
  library(DT)
  library(bslib)
  library(broom)
  library(ggplot2)
  library(base64enc)
  library(mgcv)
  library(RColorBrewer)
  library(tidyr)
  library(purrr)
  library(agricolae)
  library(drc)
  library(cowplot)
  library(MASS)
  library(Matrix)
  library(shinyjs)
  library(equatiomatic)
  library(car)
  files <- list.files(".", full.names = TRUE)
  files <- files[!(basename(files) %in% c("app.R", "www"))]
  lapply(files, source)
  Sys.setenv(RUN_MODE = "BROWSER")
  app <- app()
  shiny::shinyApp(app$ui, app$server)
}
code <- body(code) |> deparse()
code <- code[2:(length(code) - 1)]
writeLines(code, con)
close(con)

setwd("/home/konrad/Documents/Biostats/app")
shinylive::export(appdir = "./app/", destdir = "./app", quiet = FALSE)
httpuv::runStaticServer(".")
