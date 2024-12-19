# Copy R files from bs
# ========================================
files <- list.files("./bs/R", pattern = ".R", full.names = TRUE)
file.copy(files, "./app/", overwrite = TRUE)

file <- "./app/MainApp.R"
content <- readLines(file)
content <- gsub("uploadUIField",
                "fileInput(\"file\", \"Choose CSV File\",\n        accept = c(\n            \"text/csv\",\n            \"text/comma-separated-values,text/plain\",\n            \".csv\"\n          )\n        ),\n", 
                content)
writeLines(content, file)


# Copy www files from bs
# ========================================
destination_folder <- "./app/www/"
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)
}
file.copy(
  from = list.files("./bs/inst/www", full.names = TRUE),
  to = destination_folder,
  recursive = TRUE, overwrite = TRUE
)

# Create app.R
# ========================================
file.create("./app/app.R", overwrite = TRUE)
con <- file("./app/app.R")
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
# shinylive::export(
#   appdir = "./app",
#   destdir = "./app/serverless/",
#   quiet = FALSE
# )
shinylive::export("./app", "_site")

# Start app
# ========================================
# httpuv::runStaticServer("./app/serverless/")
