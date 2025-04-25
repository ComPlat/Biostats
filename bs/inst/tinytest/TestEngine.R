library(bs)
library(tinytest)

# Test create formula
# =======================================================================================
test_create_formula_V1_2 <- function() {
  df <- CO2

  DataModelState <- new.env()
  DataModelState$formula <- NULL

  cf <- bs:::create_formula_V1_2$new(
    response_var = "uptake",
    right_site = "conc",
    df = df,
    com = bs:::backend_communicator_V1_2
  )

  eq <- cf$eval(DataModelState)

  expect_equal(DataModelState$formula, as.formula("uptake ~ conc"))
  expect_true(is.character(eq)) # extract_eq() returns a LaTeX-like string
}
test_create_formula_V1_2()


# Test correlation_V1_2
# =======================================================================================
test_corr <- function() {
  formula <- as.formula("uptake ~ conc")
  corr <- bs:::correlation_V1_2$new(CO2, formula, "pearson", "two.sided",
    0.95, bs:::backend_communicator_V1_2)
  trash <- corr$validate()
  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0
  corr$eval(ResultsState)
  ResultsState |> ls()
  ResultsState$all_data
  expect_equal(
    ResultsState$all_data[[1]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "pearson",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
  )

  corr$method <- "kendall"
  corr$eval(ResultsState)
  expect_equal(
    ResultsState$all_data[[2]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "kendall",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
  )

  corr$method <- "spearman"
  corr$eval(ResultsState)
  expect_equal(
    ResultsState$all_data[[3]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "spearman",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
  )

  corr$method <- "spearman"
  corr$conflevel <- 0.5
  corr$eval(ResultsState)
  expect_equal(
    ResultsState$all_data[[4]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "spearman",
        alternative = "two.sided",
        conf.level = 0.5
      )
    )
  )

  corr$method <- "pearson"
  corr$conflevel <- 0.5
  corr$alternative <- "less"
  corr$eval(ResultsState)
  expect_equal(
    ResultsState$all_data[[5]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "pearson",
        alternative = "less",
        conf.level = 0.5
      )
    )
  )
}
test_corr()

# Test visualisation_V1_2
# =======================================================================================
test_vis_all <- function() {
  for (method in c("box", "dot", "line")) {
    vis <- bs:::visualisation_V1_2$new(
      df = CO2,
      x = "conc",
      y = "uptake",
      method = method,
      xlabel = paste("xlabel", method),
      type_of_x = if (method == "box") "factor" else "numeric",
      ylabel = paste("ylabel", method),
      colour_var = "",
      colour_legend_title = "Legend colour",
      colour_theme = "Dark2",
      fill_var = "",
      fill_legend_title = "Legend fill",
      fill_theme = "YlOrRd",
      facet_var = if (method == "dot") "Treatment" else "",
      facet_y_scaling = "fixed",
      xrange = c(0, 1000),
      yrange = c(0, 60),
      width = 20,
      height = 20,
      resolution = 150,
      com = bs:::backend_communicator_V1_2
    )

    ResultsState <- new.env()
    ResultsState$all_data <- list()
    ResultsState$counter <- 0
    ResultsState$history <- list()

    p <- vis$eval(ResultsState)

    # Basic checks
    expect_true(inherits(p, "gg"))
    expect_equal(length(ResultsState$all_data), 1)
    expect_equal(length(ResultsState$history), 1)

    # History correctness
    h <- ResultsState$history[[1]]
    expect_equal(h$type, "Visualisation")
    expect_equal(h$`Plot-type`, method)
    expect_equal(h$`X axis label`, paste("xlabel", method))
    expect_equal(h$`Y axis label`, paste("ylabel", method))

    # Result name check
    expect_match(names(ResultsState$all_data)[1], paste0("1 Visualization ", 
      c(box = "Boxplot", dot = "Scatterplot", line = "Lineplot")[method]))
  }
}
test_vis_all()

test_vis_warn_size <- function() {
  vis <- bs:::visualisation_V1_2$new(
    df = CO2, x = "conc", y = "uptake", method = "box",
    xlabel = "x", type_of_x = "factor", ylabel = "y",
    colour_var = "", colour_legend_title = "", colour_theme = "Accent",
    fill_var = "", fill_legend_title = "", fill_theme = "BuGn",
    facet_var = "", facet_y_scaling = "free",
    xrange = c(0, 100), yrange = c(0, 100),
    width = -5, height = 200, resolution = 72,
    com = bs:::backend_communicator_V1_2
  )
  vis$validate()
  expect_equal(vis$width, 10)
  expect_equal(vis$height, 100)
}
test_vis_warn_size()


# Filter data
# =======================================================================================
test_apply_filter_V1_2 <- function() {
  df <- CO2
  ResultsState <- new.env()
  ResultsState$history <- list()

  DataModelState <- new.env()
  DataModelState$df <- df

  af <- bs:::apply_filter_V1_2$new("Plant", c("Qn1", "Qn2"), bs:::backend_communicator_V1_2)
  af$validate()
  af$eval(DataModelState, ResultsState)

  expect_true("Qn1" %in% unique(DataModelState$df$Plant))
  expect_true("Qn2" %in% unique(DataModelState$df$Plant))
  expect_equal(DataModelState$filter_col, "Plant")
  expect_equal(DataModelState$filter_group, c("Qn1", "Qn2"))
  expect_equal(ResultsState$history[[1]]$type, "ApplyFilter")
}
test_apply_filter_V1_2()
test_remove_filter_V1_2 <- function() {
  df <- CO2
  DataModelState <- new.env()
  DataModelState$df <- df[CO2$Plant %in% c("Qn1", "Qn2"), ]
  DataModelState$backup_df <- df
  DataModelState$filter_col <- "Plant"
  DataModelState$filter_group <- c("Qn1", "Qn2")

  ResultsState <- new.env()
  ResultsState$history <- list()

  rf <- bs:::remove_filter_V1_2$new()
  rf$eval(ResultsState, DataModelState)

  expect_equal(nrow(DataModelState$df), nrow(CO2))
  expect_null(DataModelState$backup_df)
  expect_equal(ResultsState$history[[1]]$type, "RemoveFilter")
}
test_remove_filter_V1_2()

# Data wrangling
# =======================================================================================
test_create_intermediate_var_V1_2 <- function() {
  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  DataWranglingState <- new.env()
  DataWranglingState$intermediate_vars <- list()

  ci <- bs:::create_intermediate_var_V1_2$new(
    df = CO2,
    df_name = "CO2",
    intermediate_vars = list(),
    operation = "uptake + conc",
    name = "myVar",
    com = bs:::backend_communicator_V1_2
  )

  ci$validate()
  ci$eval(ResultsState, DataWranglingState)

  expect_true("myVar" %in% names(DataWranglingState$intermediate_vars))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "CreateIntermediateVariable")
}
test_create_intermediate_var_V1_2()

test_create_new_col_V1_2 <- function() {
  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  DataWranglingState <- new.env()
  DataWranglingState$df <- CO2
  DataWranglingState$intermediate_vars <- list()
  DataWranglingState$counter_id <- 0

  DataModelState <- new.env()
  DataModelState$df <- CO2
  DataModelState$backup_df <- CO2

  cc <- bs:::create_new_col_V1_2$new(
    df = CO2,
    df_name = "CO2",
    intermediate_vars = list(),
    operation = "uptake / conc",
    name = "ratio",
    com = bs:::backend_communicator_V1_2
  )

  cc$validate()
  cc$eval(ResultsState, DataWranglingState, DataModelState)

  expect_true("ratio" %in% colnames(DataModelState$df))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "CreateNewColumn")
}
test_create_new_col_V1_2()

test_remove_intermediate_var_V1_2 <- function() {
  ResultsState <- new.env()
  ResultsState$history <- list()

  DataWranglingState <- new.env()
  DataWranglingState$intermediate_vars <- list(foo = 1:3)

  riv <- bs:::remove_intermediate_var_V1_2$new("foo", com = bs:::backend_communicator_V1_2)
  riv$eval(ResultsState, DataWranglingState)

  expect_null(DataWranglingState$intermediate_vars$foo)
  expect_equal(ResultsState$history[[1]]$type, "RemoveIntermediateVariable")
}
test_remove_intermediate_var_V1_2()

# Statistical tests
# =======================================================================================
test_t_test_V1_2 <- function() {
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  tt <- bs:::t_test_V1_2$new(
    df = df,
    formula = formula,
    variances_equal = "eq",
    conf_level = 0.95,
    alternative_hyp = "two.sided",
    com = bs:::backend_communicator_V1_2
  )

  result <- tt$eval(ResultsState)

  expect_true("estimate" %in% colnames(result))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "TTest")
}
test_t_test_V1_2()

test_statistical_methods <- function() {
  df <- CO2
  df$group <- rep(c("A", "B", "C", "D"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  methods <- c("aov", "kruskal", "HSD", "kruskalTest", "LSD", "scheffe", "REGW")

  for (method in methods) {
    ResultsState$all_data <- list()
    ResultsState$history <- list()
    ResultsState$counter <- 0

    st <- bs:::statistical_tests_V1_2$new(
      df = df,
      formula = formula,
      balanced_design = "Balanced",
      p_val = 0.05,
      p_val_adj_method = "bonferroni",
      com = bs:::backend_communicator_V1_2
    )

    result <- st$eval(ResultsState, method = method)

    expect_true(is.data.frame(result) || is.matrix(result))
    expect_equal(ResultsState$counter, 1)
    expect_match(ResultsState$history[[1]]$type, "ANOVA|Test|Tukey|post hoc|Least|Scheffe|REGW")
    expect_true(length(ResultsState$all_data) == 1)
  }
}
test_statistical_methods()

# Assumptions
# =======================================================================================
test_shapiro_on_data_V1_2 <- function() {
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  sh <- bs:::shapiro_on_data_V1_2$new(df = df, formula = formula, com = bs:::backend_communicator_V1_2)
  result <- sh$eval(ResultsState)

  expect_true(is.data.frame(result))
  expect_true("Normal distributed" %in% colnames(result))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "ShapiroOnData")
}
test_shapiro_on_data_V1_2()
test_shapiro_on_residuals_V1_2 <- function() {
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  sh <- bs:::shapiro_on_residuals_V1_2$new(df = df, formula = formula, com = bs:::backend_communicator_V1_2)
  result <- sh$eval(ResultsState)

  expect_true(is.data.frame(result))
  expect_true("Residuals normal distributed" %in% colnames(result))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "ShapiroOnResiduals")
}
test_shapiro_on_residuals_V1_2()
test_levene_V1_2 <- function() {
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  lv <- bs:::levene_V1_2$new(df = df, formula = formula, center = "mean", com = bs:::backend_communicator_V1_2)
  result <- lv$eval(ResultsState)

  expect_true(is.data.frame(result))
  expect_true("Variance homogenity" %in% colnames(result))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "LeveneTest")
}
test_levene_V1_2()
test_diagnostic_plots_V1_2 <- function() {
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0

  dp <- bs:::diagnostic_plots_V1_2$new(df = df, formula = formula, com = bs:::backend_communicator_V1_2)
  p <- dp$eval(ResultsState)

  expect_true(inherits(p, "gg"))
  expect_match(names(ResultsState$all_data), "DiagnosticPlotNr1")
  expect_equal(ResultsState$history[[1]]$type, "DiagnosticPlots")
}
test_diagnostic_plots_V1_2()

# Dose response
# =======================================================================================
test_dose_response_V1_2 <- function() {
  df <- data.frame(
    dose = rep(c(0.1, 1, 10, 100), each = 5),
    response = c(100, 90, 80, 60, 50, 98, 85, 75, 58, 48, 95, 80, 70, 55, 45, 92, 78, 65, 50, 40),
    name = rep(c("A", "B"), length.out = 20)
  )
  formula <- response ~ dose

  ResultsState <- new.env()
  ResultsState$all_data <- list()
  ResultsState$history <- list()
  ResultsState$counter <- 0
  ResultsState$all_names <- list()

  dr <- bs:::dose_response_V1_2$new(
    df = df,
    outliers = NULL,
    is_xlog = FALSE,
    is_ylog = FALSE,
    substance_names = "name",
    formula = formula,
    com = bs:::backend_communicator_V1_2
  )

  res <- dr$eval(ResultsState)

  expect_true(is.list(res))
  expect_equal(ResultsState$counter, 1)
  expect_equal(ResultsState$history[[1]]$type, "DoseResponse")
}
test_dose_response_V1_2()

# Remove result from ResultList
# =======================================================================================
test_remove_result_V1_2 <- function() {
  ResultsState <- new.env()
  ResultsState$all_data <- list("MyResult" = 42)
  ResultsState$history <- list()

  rr <- bs:::remove_result_V1_2$new("MyResult")
  rr$eval(ResultsState)

  expect_null(ResultsState$all_data$MyResult)
  expect_equal(ResultsState$history[[1]]$type, "RemoveResult")
}
test_remove_result_V1_2()
