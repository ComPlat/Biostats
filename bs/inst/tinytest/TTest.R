library(shinytest2)
library(tinytest)
app <- bs::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "bs")
)
app$set_window_size(width = 2259, height = 1326)
app$set_inputs(conditionedPanels = "Tests")
app$click("TESTS-open_formula_editor")
app$set_inputs(`FO-colnames-dropdown_0` = "uptake")
app$click("FO-colnames_Treatment_0")
app$click("FO-create_formula")
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")

app$click("TESTS-tTest")
res <- app$get_values()$export
CO2$Treatment <- as.character(CO2$Treatment)
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment, data = CO2,
    var.equal = TRUE, conf.level = 0.95,
    alternative = "two.sided"
  )
)
tinytest::expect_equal(res[[1]], expected)
# Update output value
app$set_inputs(`TESTS-altHyp` = "less")
app$click("TESTS-tTest")
res <- app$get_values()$export
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment, data = CO2,
    var.equal = TRUE, conf.level = 0.95,
    alternative = "less"
  )
)
tinytest::expect_equal(res[[1]], expected)
# Update output value
app$set_inputs(`TESTS-altHyp` = "greater")
app$click("TESTS-tTest")
res <- app$get_values()$export
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment, data = CO2,
    var.equal = TRUE, conf.level = 0.95,
    alternative = "greater"
  )
)
tinytest::expect_equal(res[[1]], expected)
# Update output value
app$set_inputs(`TESTS-varEq` = "noeq")
app$click("TESTS-tTest")
res <- app$get_values()$export
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment, data = CO2,
    var.equal = FALSE, conf.level = 0.95,
    alternative = "greater"
  )
)
tinytest::expect_equal(res[[1]], expected)
app$stop()
