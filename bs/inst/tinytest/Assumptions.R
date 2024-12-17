library(shinytest2)
library(tinytest)
app <- bs::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$wait_for_idle()
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "bs")
)
app$wait_for_idle()
app$set_inputs(conditionedPanels = "Assumption")
app$wait_for_idle()
app$set_window_size(width = 2259, height = 1326)
app$wait_for_idle()
app$click("open_formula_editor")
app$wait_for_idle()
app$set_inputs(`FO-colnames-dropdown_0` = "uptake")
app$wait_for_idle()
app$click("FO-colnames_Treatment_0")
app$wait_for_idle()
app$click("FO-create_formula")
app$wait_for_idle()
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
app$wait_for_idle()
app$click("ASS-shapiro")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
expected <- rbind(
  broom::tidy(shapiro.test(CO2[CO2$Treatment == "nonchilled", "uptake"])),
  broom::tidy(shapiro.test(CO2[CO2$Treatment == "chilled", "uptake"]))
)
expected$variable <- c("nonchilled", "chilled")
expected$`Normal distributed` <- expected$p.value > 0.05
res
tinytest::expect_equal(res[[1]], expected)

# Update output value
app$click("ASS-shapiroResiduals")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
fit <- lm(uptake ~ Treatment, data = CO2)
r <- resid(fit)
expected <- broom::tidy(shapiro.test(r))
expected$`Residuals normal distributed` <- expected$p.value > 0.05
tinytest::expect_equal(res[[1]], expected)

# Update output value
app$click("ASS-levene")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
expected <- broom::tidy(car::leveneTest(uptake ~ Treatment,
  data = CO2, center = "mean"
))
expected$`Variance homogenity` <- expected$p.value > 0.05
tinytest::expect_equal(res[[1]], expected)

# Update output value
app$click("ASS-DiagnosticPlot")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
tinytest::expect_equal(inherits(res[[1]], "ggplot"), TRUE)
# TODO: add internal backend test for diagnostic plot functions

app$wait_for_idle()
app$stop()
