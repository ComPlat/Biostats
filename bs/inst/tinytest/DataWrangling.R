library(shinytest2)
library(tinytest)
library(bs)

# Sys.setenv(RUN_MODE = "BROWSER") # SERVER
app <- bs::app()
# Create app
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$wait_for_idle()
app$set_window_size(1500, 1000)
app$wait_for_idle()

# Upload test file
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "bs")
)
app$wait_for_idle()

# Switch to data wrangling
app$set_inputs(`conditionedPanels` = "DataWrangling")
app$wait_for_idle()

# Seq tests
app$click("OP-seq")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
app$set_inputs(
  `OP-editable_code` = 
  paste0(
    content, "1, 100, 1"
  )
)
app$click("OP-bracket_close")
app$wait_for_idle()
app$set_inputs(`OP-iv` = "Seq")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " Seq(1, 100, 1 )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$Seq, seq(1, 100, 1))

# dataframe tests
app$set_inputs(`OP-editable_code` = "")
app$click("OP-df")
app$wait_for_idle()
app$click("OP-colnames_conc_0")
app$click("OP-comma")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
content <- app$get_values()$input[["OP-editable_code"]]
app$wait_for_idle()
app$set_inputs(`OP-iv` = "df_new")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " DataFrame( conc , conc )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
df_new <- data.frame(CO2$conc, CO2$conc)
names(df_new) <- c("conc", "conc")
expect_equal(iv_list$df_new, df_new)
app$set_inputs(`OP-editable_code` = "")

# random tests
random_funcs <- c(
  "dnorm", "pnorm", "qnorm",
  "dbinom", "pbinom", "qbinom",
  "dpois", "ppois",
  "dunif", "punif", "qunif"
)
args <- list(
  0, 0, 0.2,
  "0, 10, 0.5", "0, 10, 0.5", "0, 10, 0.5",
  "0, 2", "0, 2",
  "0, 0, 2", "0.2, 0, 3", "0.2, 0, 3"
)
for (i in seq_along(random_funcs)) {
  expr <- paste0(random_funcs[i], "(", args[[i]], ")")
  # print(expr)
  res <- eval(parse(text = expr))
  func <- paste0("OP-", random_funcs[i])
  app$set_inputs(`OP-editable_code` = "")
  app$wait_for_idle()
  app$click(paste0(func))
  app$wait_for_idle()
  app$set_inputs(
    `OP-editable_code` =
      paste0(app$get_values()$input[["OP-editable_code"]], args[[i]])
  )
  app$wait_for_idle()
  app$click("OP-bracket_close")
  app$wait_for_idle()
  app$set_inputs(`OP-iv` = func)
  app$wait_for_idle()
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  output <- iv_list[[make.names(func)]]
  expect_equal(output, res) |> print()
}

# NOTE: this is necessary as no seed can be set
validate_distribution <- function(values, dist, ...) {
  if (dist == "normal") {
    expect_true(abs(mean(values) - 0) < 0.1, info = "Normal mean should be close to 0")
    expect_true(abs(sd(values) - 1) < 0.1, info = "Normal sd should be close to 1")
  } else if (dist == "uniform") {
    expect_true(all(values >= 0 & values <= 1), info = "Uniform values should be in [0,1]")
    expect_true(abs(mean(values) - 0.5) < 0.1, info = "Uniform mean should be close to 0.5")
  } else if (dist == "pois") {
    expect_true(abs(mean(values) - 2) < 0.1, info = "Uniform mean should be close to 0.5")
    # NOTE: set lambda to 2
  } else if (dist == "binom") {
    # Assuming size = 10 trials, and probability of success = 0.5
    n_trials <- 10
    prob_success <- 0.5
    expect_true(abs(mean(values) - (n_trials * prob_success)) < 0.5,
      info = "Binomial mean should be close to n * p"
    ) |> print()
    expect_true(abs(var(values) - (n_trials * prob_success * (1 - prob_success))) < 0.5,
      info = "Binomial variance should be close to n * p * (1 - p)"
    ) |> print()
  }
}
random_funcs <- c(
  "norm", "unif", "pois", "binom"
)
args <- list(
  "10000", "10001", "10002, 2", "10000, 10, 0.5"
)
for (i in seq_along(random_funcs)) {
  func <- paste0("OP-r", random_funcs[i])
  app$set_inputs(`OP-editable_code` = "")
  app$wait_for_idle()
  app$click(paste0(func))
  app$wait_for_idle()
  app$set_inputs(
    `OP-editable_code` =
      paste0(app$get_values()$input[["OP-editable_code"]], args[[i]])
  )
  app$wait_for_idle()
  app$click("OP-bracket_close")
  app$wait_for_idle()
  app$set_inputs(`OP-iv` = func)
  app$wait_for_idle()
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  app$wait_for_idle()
  output <- iv_list[[make.names(func)]]
  print(func)
  str(output)
  validate_distribution(output, random_funcs[i])
}
app$set_inputs(`OP-editable_code` = "")
app$wait_for_idle()

# Test arithmetic operations
app$click("OP-colnames_conc_0")
app$wait_for_idle()
app$click("OP-add")
app$wait_for_idle()
app$click("OP-colnames_conc_0")
app$wait_for_idle()
app$set_inputs(`OP-iv` = "Plus")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc + conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$Plus, CO2$conc + CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-sub")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "MINUS")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc - conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MINUS, CO2$conc - CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-div")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "DIVIDE")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc / conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$DIVIDE, CO2$conc / CO2$conc)

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-mul")
app$click("OP-colnames_conc_0")
app$set_inputs(`OP-iv` = "MULTIPLY")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
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
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " conc + ( conc * conc )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$ARITHMETIC, CO2$conc + (CO2$conc * CO2$conc))

app$set_inputs(`OP-editable_code` = "")
app$click("OP-get_elem")
app$click("OP-colnames_conc_0")
app$click("OP-comma")
app$set_inputs(`OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], " 1"))
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "GET_ELEM_AND_COMMA")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " get_elem( conc , 1 )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$GET_ELEM_AND_COMMA, CO2$conc[1])

# Test math functions
app$set_inputs(`OP-editable_code` = "")
app$click("OP-log")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-log10")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-sqrt")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-exp")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "MATH1")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(content, " log( conc ) + log10( conc ) + sqrt( conc ) + exp( conc )")
iv_list <- app$get_values()$export[["OP-iv_list"]]
conc <- CO2$conc
expect_equal(iv_list$MATH1, log(conc) + log10(conc) + sqrt(conc) + exp(conc))

app$set_inputs(`OP-editable_code` = "")
app$click("OP-colnames_conc_0")
app$click("OP-exponent")
app$set_inputs(
  `OP-editable_code` =
    paste0(app$get_values()$input[["OP-editable_code"]], " 2")
)
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-sin")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-cos")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-tan")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-sinh")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-cosh")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-tanh")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$set_inputs(`OP-iv` = "MATH2")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]]
expect_equal(
  gsub(" ", "", content),
  "conc^(2)+sin(conc)+cos(conc)+tan(conc)+sinh(conc)+cosh(conc)+tanh(conc)"
)
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MATH2, eval(parse(text = content)))

app$set_inputs(`OP-editable_code` = "1")
app$set_inputs(`OP-iv` = "One")
app$click("OP-run_op_intermediate")
app$set_inputs(`OP-editable_code` = "")
app$click("OP-asin")
app$wait_for_idle()
app$click("OP-intermediate_vars_One_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-add")
app$click("OP-acos")
app$click("OP-intermediate_vars_One_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-add")
app$click("OP-atan")
app$click("OP-intermediate_vars_One_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$set_inputs(`OP-iv` = "MATH3")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
app$click("OP-run_op_intermediate")
expect_equal(
  content,
  " asin( One ) + acos( One ) + atan( One )"
)
iv_list <- app$get_values()$export[["OP-iv_list"]]
One <- 1
print(content)
expect_equal(iv_list$MATH3, eval(parse(text = content)))


app$set_inputs(`OP-editable_code` = "-1.5")
app$set_inputs(`OP-iv` = "Num")
app$click("OP-run_op_intermediate")
app$wait_for_idle()
app$set_inputs(`OP-editable_code` = "")
app$click("OP-abs")
app$wait_for_idle()
app$click("OP-intermediate_vars_Num_0")
app$wait_for_idle()
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-add")
app$click("OP-ceil")
app$click("OP-intermediate_vars_Num_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-add")
app$click("OP-floor")
app$click("OP-intermediate_vars_Num_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-add")
app$click("OP-trunc")
app$click("OP-intermediate_vars_Num_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-add")
app$click("OP-round")
app$click("OP-intermediate_vars_Num_0")
app$click("OP-bracket_close")
app$wait_for_idle()
app$set_inputs(`OP-iv` = "MATH4")
app$wait_for_idle()
content <- app$get_values()$input[["OP-editable_code"]]
app$wait_for_idle()
app$click("OP-run_op_intermediate")
expect_equal(
  content, " abs( Num ) + ceiling( Num ) + floor( Num ) + trunc( Num ) + round( Num )"
)
iv_list <- app$get_values()$export[["OP-iv_list"]]
Num <- -1.5
expect_equal(iv_list$MATH4, eval(parse(text = content)))

# Test comparison operations
app$set_inputs(`OP-editable_code` = "C(1, 2, 2, 2)")
app$set_inputs(`OP-iv` = "Vec1")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
app$set_inputs(`OP-editable_code` = "C(1, 2, 3, 4)")
app$set_inputs(`OP-iv` = "Vec2")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
Vec1 <- c(1, 2, 2, 2)
Vec2 <- c(1, 2, 3, 4)
operations <- c(
  "OP-eq", "OP-not_eq", "OP-larger",
  "OP-smaller", "OP-larger_eq", "OP-smaller_eq"
)
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  app$click("OP-intermediate_vars_Vec1_0")
  app$wait_for_idle()
  app$click(i)
  app$wait_for_idle()
  app$click("OP-intermediate_vars_Vec2_0")
  app$wait_for_idle()
  app$set_inputs(`OP-iv` = i)
  app$wait_for_idle()
  content <- app$get_values()$input[["OP-editable_code"]]
  app$wait_for_idle()
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  # print(iv_list[[make.names(i)]])
  expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))) |> print()
}

# Test statistical operations and utilities
operations <- c(
  "OP-mean", "OP-median", "OP-min",
  "OP-max", "OP-sum", "OP-sd"
)
Mean <- bs:::Mean
Median <- bs:::Median
Min <- bs:::Min
Max <- bs:::Max
Sum <- bs:::Sum
SD <- bs:::SD
conc <- CO2$conc
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  app$click(i)
  app$set_inputs(`OP-iv` = i)
  app$click("OP-colnames_conc_0")
  app$click("OP-bracket_close")
  app$wait_for_idle()
  content <- app$get_values()$input[["OP-editable_code"]]
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  # print(iv_list[[make.names(i)]])
  expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))) |> print()
}

app$set_inputs(`OP-editable_code` = "")
app$click("OP-get_rows")
app$set_inputs(`OP-iv` = "get_rows")
app$click("OP-colnames_df_0")
app$click("OP-comma")
app$set_inputs(`OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], " conc == 95"))
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list[["get_rows"]][, 6], CO2[conc == 95, 5]) # NOTE: 6 and 5 as row names are read in as own column


app$set_inputs(`OP-editable_code` = "")
app$click("OP-get_cols")
app$wait_for_idle()
app$set_inputs(`OP-iv` = "get_cols")
app$wait_for_idle()
app$click("OP-colnames_df_0")
app$wait_for_idle()
app$click("OP-comma")
app$wait_for_idle()
app$click("OP-colnames_conc_0")
app$wait_for_idle()
app$click("OP-comma")
app$wait_for_idle()
app$click("OP-colnames_conc_0")
app$wait_for_idle()
app$click("OP-comma")
app$wait_for_idle()
app$click("OP-colnames_uptake_0")
app$wait_for_idle()
app$click("OP-bracket_close")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list[["get_cols"]], CO2[, c("conc", "conc", "uptake")])


# Test string functions
app$set_inputs(`OP-editable_code` = 'C("A", "B", "C")')
app$wait_for_idle()
app$set_inputs(`OP-iv` = "S1")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
app$set_inputs(`OP-editable_code` = 'C("d", "e", "f")')
app$wait_for_idle()
app$set_inputs(`OP-iv` = "S2")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
S1 <- c("A", "B", "C")
S2 <- c("d", "e", "f")
operations <- c(
  "OP-paste", "OP-paste0"
)
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  app$wait_for_idle()
  app$click(i)
  app$wait_for_idle()
  app$click("OP-intermediate_vars_S1_0")
  app$wait_for_idle()
  app$click("OP-comma")
  app$wait_for_idle()
  app$click("OP-intermediate_vars_S2_0")
  app$wait_for_idle()
  app$click("OP-bracket_close")
  app$wait_for_idle()
  app$set_inputs(`OP-iv` = i)
  app$wait_for_idle()
  content <- app$get_values()$input[["OP-editable_code"]]
  app$wait_for_idle()
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  # print(iv_list[[make.names(i)]])
  expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))) |> print()
}
operations <- c(
  "OP-tolower", "OP-toupper"
)
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  app$wait_for_idle()
  app$click(i)
  app$wait_for_idle()
  app$click("OP-intermediate_vars_S1_0")
  app$wait_for_idle()
  app$click("OP-bracket_close")
  app$wait_for_idle()
  app$set_inputs(`OP-iv` = i)
  app$wait_for_idle()
  content <- app$get_values()$input[["OP-editable_code"]]
  app$wait_for_idle()
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  # print(iv_list[[make.names(i)]])
  expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))) |> print()
}

# Test casts
app$set_inputs(`OP-editable_code` = 'C("10.5", "1.4", "1.3", 3.14)')
app$wait_for_idle()
app$set_inputs(`OP-iv` = "S3")
app$wait_for_idle()
app$click("OP-run_op_intermediate")
app$wait_for_idle()
S3 <- c("10.5", "1.4", "1.3", 3.14)
operations <- c(
  "OP-as_int", "OP-as_real", "OP-as_fact", "OP-as_char"
)
as.int <- as.integer
as.real <- as.numeric
as.fact <- as.factor
as.char <- as.character
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  app$wait_for_idle()
  app$click(i)
  app$wait_for_idle()
  app$click("OP-intermediate_vars_S3_0")
  app$wait_for_idle()
  app$click("OP-bracket_close")
  app$wait_for_idle()
  app$set_inputs(`OP-iv` = i)
  app$wait_for_idle()
  content <- app$get_values()$input[["OP-editable_code"]]
  app$wait_for_idle()
  app$click("OP-run_op_intermediate")
  app$wait_for_idle()
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))) |> print()
}
app$stop()
