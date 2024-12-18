# Copy R files from bs
# ========================================
files <- list.files("/home/konrad/Documents/Biostats/bs/R/", pattern = ".R", full.names = TRUE)
file.copy(files, "/home/konrad/Documents/Biostats/app/", overwrite = TRUE)

# Copy www files from bs
# ========================================
destination_folder <- "/home/konrad/Documents/Biostats/app/www/"
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)
}
file.copy(
  from = list.files("/home/konrad/Documents/Biostats/bs/inst/www", full.names = TRUE),
  to = destination_folder,
  recursive = TRUE, overwrite = TRUE
)

# Create app.R
# ========================================
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

# Build serverless app
# ========================================
destination_folder <- "/home/konrad/Documents/Biostats/serverless/"
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)
}

setwd("/home/konrad/Documents/Biostats")
shinylive::export(
  appdir = "./app",
  destdir = "./app/serverless/",
  quiet = FALSE
)

# Start app
# ========================================
httpuv::runStaticServer("./app/serverless/")
