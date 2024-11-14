Sys.setenv(RUN_MODE = "SERVER")
library(bs)
app <- bs::app()
shiny::shinyApp(app$ui, app$server)
