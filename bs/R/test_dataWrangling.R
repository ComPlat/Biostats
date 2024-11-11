library(shinytest2)
library(tinytest)

source("MainApp.R")
Sys.setenv(RUN_MODE = "BROWSER") # SERVER

# Create app
app <- shinyApp(ui, server)
app <- AppDriver$new(app)
app$set_window_size(1500, 1000)

# Upload test file
app$upload_file(
  file = "./test_data/CO2.csv"
)
app$get_screenshot()

# Switch to data wrangling
app$set_inputs(`conditionedPanels` = "DataWrangling")
app$get_screenshot()

# Test arithmetic operations
app$click("OP-colnames_conc_0")
app$click("OP-add")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "Plus")
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc + conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$Plus, CO2$conc + CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-sub")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "MINUS")
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc - conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MINUS, CO2$conc - CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-div")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "DIVIDE")
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc / conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$DIVIDE, CO2$conc / CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-mul")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "MULTIPLY")
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc * conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MULTIPLY, CO2$conc * CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-add")
app$click("OP-bracket_open")
app$click("OP-colnames_conc_0")
app$click("OP-mul")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "ARITHMETIC")
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc + ( conc * conc )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$ARITHMETIC, CO2$conc + (CO2$conc * CO2$conc))

app$set_inputs(`OP-editable_code` = "")
app$get_screenshot()
app$click("OP-get_elem")
app$click("OP-colnames_conc_0")
app$click("OP-comma")
app$set_inputs(`OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], " 1"))
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "GET_ELEM_AND_COMMA")
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " get_elem( conc , 1 )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$GET_ELEM_AND_COMMA, get_elem(CO2$conc, 1))

app$stop()
