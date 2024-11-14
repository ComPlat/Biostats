Sys.setenv(RUN_MODE = "BROWSER")
app <- bs::app()
shiny::shinyApp(app$ui, app$server)
