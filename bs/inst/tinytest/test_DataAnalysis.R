library(shinytest2)
library(tinytest)
library(bs)

Sys.setenv(RUN_MODE = "BROWSER") # SERVER
app <- bs::app()
# Create app
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$set_window_size(1500, 1000)
# Upload test file
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "bs")
)
app$get_values()$input |> names()
app$get_values()$output |> names()

# Visualisation
app$set_inputs(`conditionedPanels` = "Visualisation")
app$set_inputs(`VIS-yVar` = "uptake")
app$set_inputs(`VIS-xVar` = "conc")
app$click("VIS-CreatePlotBox")
app$get_screenshot()

app$run_js("$('#VIS-yVar').val('uptake').trigger('change');")
app$run_js("$('#VIS-xVar').val('uptake').trigger('change');")


app$view()


app$stop()
