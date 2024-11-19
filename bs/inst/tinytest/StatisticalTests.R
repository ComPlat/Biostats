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

# Define formula
app$click("TESTS-open_formula_editor")
app$set_inputs(`FO-colnames-dropdown_0` = "uptake")
app$set_inputs(`FO-editable_code` = "conc * Treatment + Type")
app$click("FO-create_formula")
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")

# ANOVA
app$set_inputs(TestsConditionedPanels  = "More than two groups")
app$click("TESTS-aovTest")
res <- app$get_values()$export
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
expected <- broom::tidy(aov(uptake ~ conc*Treatment + Type, data = CO2))
expected <- cbind(expected, row.names(expected))
names(expected)[ncol(expected)] <- paste0("conc * Treatment + Type", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# Kruskal-Wallis
app$click("TESTS-open_formula_editor")
app$set_inputs(`FO-colnames-dropdown_0` = "uptake")
app$set_inputs(`FO-editable_code` = "conc")
app$click("FO-create_formula")
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")

app$click("TESTS-kruskalTest")
res <- app$get_values()$export
CO2$Treatment <- as.character(CO2$Treatment)
expected <- broom::tidy(kruskal.test(uptake ~conc, data = CO2))
expected <- cbind(expected, row.names(expected))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# PostHoc tests
# TukeyHSD
app$set_inputs(TestsConditionedPanels  = "Posthoc tests")
app$set_inputs(`TESTS-PostHocTests` = "HSD")
app$click("TESTS-PostHocTest")
res <- app$get_values()$export
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::HSD.test(fit,
  trt = "conc",
  alpha = 0.05, group = TRUE, unbalanced = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

app$set_inputs(TestsConditionedPanels  = "Posthoc tests")
app$set_inputs(`TESTS-PostHocTests` = "HSD")
app$set_inputs(`TESTS-design` = "ub")
app$click("TESTS-PostHocTest")
res <- app$get_values()$export
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
app$click("TESTS-PostHocTest")
res <- app$get_values()$export
df <- CO2
dep <- "uptake"
fit <- with(df, agricolae::kruskal(df[, dep], df[, "conc"]),
  alpha = 0.05, p.adj = 0.05, group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
tinytest::expect_equal(res[[1]], expected)

# LSD


# scheffe


# REGW

app$view()

