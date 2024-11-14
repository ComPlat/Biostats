Sys.setenv(RUN_MODE = "BROWSER")
library(bs)
app <- bs::app()
shiny::shinyApp(app$ui, app$server)
