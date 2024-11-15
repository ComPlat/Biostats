detach("package:bs", unload = TRUE)
install.packages("bs", repos = NULL, type = "source")
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

# Visualisation
app$set_inputs(`conditionedPanels` = "Visualisation")
app$set_inputs(`VIS-yVar` = "uptake")
app$set_inputs(`VIS-yaxisText` = "The y axis title")
app$set_inputs(`VIS-xVar` = "conc")
app$set_inputs(`VIS-xaxisText` = "The x axis title")
app$set_inputs(`VIS-fill` = "Treatment")
app$set_inputs(`VIS-legendTitleFill` = "The fill legend title")
app$set_inputs(`VIS-col` = "Treatment")
app$set_inputs(`VIS-legendTitleCol` = "The colour legend title")
app$set_inputs(`VIS-facetBy` = "Type")
app$set_inputs(`VIS-facetScales` = "fixed")
app$set_inputs(`VIS-theme` = "Dark2")
app$set_inputs(`VIS-themeFill` = "Greys")
app$set_inputs(`VIS-XRange` = "0, 200")
app$click("VIS-CreatePlotBox", wait_ = TRUE)
app$get_values()$export$`VIS-plot`

app$get_logs()


app$stop()





record_test(app = system.file("/tinytest/", package = "bs"))

