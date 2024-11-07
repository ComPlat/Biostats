source("MainApp.R")

# TODO: define getenv as command line argument
Sys.setenv(RUN_MODE = "BROWSER") # SERVER
# Sys.setenv(RUN_MODE = "SERVER")
options(shiny.autoreload = TRUE)
shinyApp(ui, server)
