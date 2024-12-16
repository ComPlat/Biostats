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
app$set_window_size(width = 2259, height = 1326)
app$wait_for_idle()
app$set_inputs(conditionedPanels = "Tests")
app$wait_for_idle()

# Define formula
app$click("open_formula_editor")
app$wait_for_idle()
app$set_inputs(`FO-colnames-dropdown_0` = "uptake")
app$wait_for_idle()
app$set_inputs(`FO-editable_code` = "conc * Treatment + Type")
app$wait_for_idle()
app$click("FO-create_formula")
app$wait_for_idle()
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
app$wait_for_idle()

# ANOVA
app$set_inputs(TestsConditionedPanels = "More than two groups")
app$wait_for_idle()
app$click("TESTS-aovTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
expected <- broom::tidy(aov(uptake ~ conc * Treatment + Type, data = CO2))
expected <- cbind(expected, row.names(expected))
names(expected)[ncol(expected)] <- paste0("conc * Treatment + Type", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# Kruskal-Wallis
app$click("open_formula_editor")
app$wait_for_idle()
app$set_inputs(`FO-colnames-dropdown_0` = "uptake")
app$wait_for_idle()
app$set_inputs(`FO-editable_code` = "conc")
app$wait_for_idle()
app$click("FO-create_formula")
app$wait_for_idle()
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
app$wait_for_idle()

app$click("TESTS-kruskalTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
CO2$Treatment <- as.character(CO2$Treatment)
expected <- broom::tidy(kruskal.test(uptake ~ conc, data = CO2))
expected <- cbind(expected, row.names(expected))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# PostHoc tests
# TukeyHSD
app$set_inputs(TestsConditionedPanels = "Posthoc tests")
app$wait_for_idle()
app$set_inputs(`TESTS-PostHocTests` = "HSD")
app$wait_for_idle()
app$click("TESTS-PostHocTest")
app$wait_for_idle()
res <- app$get_values()$export
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::HSD.test(fit,
  trt = "conc",
  alpha = 0.05, group = TRUE, unbalanced = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

app$set_inputs(TestsConditionedPanels = "Posthoc tests")
app$wait_for_idle()
app$set_inputs(`TESTS-PostHocTests` = "HSD")
app$wait_for_idle()
app$set_inputs(`TESTS-design` = "ub")
app$wait_for_idle()
app$click("TESTS-PostHocTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::HSD.test(fit,
  trt = "conc",
  alpha = 0.05, group = TRUE, unbalanced = FALSE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# Kruskal-Wallis test
app$set_inputs(`TESTS-PostHocTests` = "kruskalTest")
app$wait_for_idle()
app$set_inputs(`TESTS-padj` = "BH")
app$wait_for_idle()
app$click("TESTS-PostHocTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
df <- CO2
dep <- "uptake"
fit <- with(df, agricolae::kruskal(df[, dep], df[, "conc"]),
  alpha = 0.05, p.adj = "BH", group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# LSD
app$set_inputs(`TESTS-PostHocTests` = "LSD")
app$wait_for_idle()
app$set_inputs(`TESTS-padj` = "BY")
app$wait_for_idle()
app$click("TESTS-PostHocTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
df <- CO2
dep <- "uptake"
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::LSD.test(
  fit,
  trt = "conc",
  alpha = 0.05, p.adj = "BY", group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# scheffe
# aov_res <- aov(formula, data = df)
# fit <- agricolae::scheffe.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
app$set_inputs(`TESTS-PostHocTests` = "scheffe")
app$wait_for_idle()
app$click("TESTS-PostHocTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::scheffe.test(
  fit,
  trt = "conc",
  alpha = 0.05, group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# REGW
app$set_inputs(`TESTS-PostHocTests` = "REGW")
app$wait_for_idle()
app$click("TESTS-PostHocTest")
app$wait_for_idle()
res <- app$get_values()$export
app$wait_for_idle()
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::REGW.test(
  fit,
  trt = "conc",
  alpha = 0.05, group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)


app$stop()
