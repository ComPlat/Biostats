library(shinytest2)
library(tinytest)
app <- bs::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$wait_for_idle()
app$upload_file(
  file = system.file("/test_data/DoseResponse.csv", package = "bs")
)
app$wait_for_idle()
app$set_window_size(width = 2259, height = 1326)
app$wait_for_idle()
app$set_inputs(conditionedPanels = "Dose Response analysis")
app$wait_for_idle()

# Define formula
app$click("DOSERESPONSE-open_formula_editor")
app$wait_for_idle()
app$set_inputs(`FO-colnames-dropdown_0` = "abs")
app$wait_for_idle()
app$set_inputs(`FO-editable_code` = "conc")
app$wait_for_idle()
app$click("FO-create_formula")
app$wait_for_idle()
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
app$wait_for_idle()

app$set_inputs(`DOSERESPONSE-substanceNames` = "names")
app$wait_for_idle()
app$click("DOSERESPONSE-ic50")
app$wait_for_idle()

res <- app$get_values()$export
res_df <- res[[1]]@df

data <- read.csv(system.file("/test_data/DoseResponse.csv", package = "bs"))
expected <- bs:::ic50(
  data, "abs", "conc",
  "names", NULL,
  FALSE, FALSE
)
dfs <- lapply(expected, function(x) {
  if (is.list(x)) {
    return(x[[1]])
  }
})
expected <- do.call(rbind, dfs)
tinytest::expect_equal(res_df, expected)

app$stop()
