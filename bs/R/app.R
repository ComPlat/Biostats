source("MainApp.R")

Sys.setenv(RUN_MODE = "BROWSER") # SERVER
shinyApp(ui, server)
