library(ggplot2)

make_loss_fn <- function(formula, df) {
  rhs <- as.character(formula)[3] |> str2lang()
  lhs <- as.character(formula)[2]
  vars <- all.vars(rhs)
  params <- setdiff(vars, names(df))

  function(par, error_calc = TRUE) {
    eval_env <- new.env()
    list2env(df, envir = eval_env)
    list2env(as.list(setNames(par, params)), envir = eval_env)
    pred <- eval(rhs, envir = eval_env)
    if (!error_calc) {
      return(pred)
    }
    actual <- df[[lhs]]
    sum((actual - pred)^2)
  }
}

find_start <- function(loss_fn, params_len, lower_boundary, upper_boundary) {
  space <- matrix(0, ncol = params_len, nrow = 400) # NOTE: adapt if required
  for (i in seq_len(nrow(space))) {
    space[i, ] <- runif(ncol(space), min = lower_boundary, max = upper_boundary)
  }
  space[which.min(apply(space, 1, loss_fn)), ]
}

optimize <- function(formula, df, lower_boundary, upper_boundary) {
  lhs <- as.character(formula)[2]
  rhs <- as.character(formula)[3] |> str2lang()
  vars <- all.vars(rhs)
  params <- setdiff(vars, names(df))

  loss_fn <- make_loss_fn(formula, df)

  start_params <- find_start(loss_fn, length(params), lower_boundary, upper_boundary)

  opti_params <- optim(par = start_params, fn = loss_fn)
  pred <- loss_fn(opti_params$par, error_calc = FALSE)

  # Compute predictions
  pred <- loss_fn(opti_params$par, error_calc = FALSE)
  df$Predicted <- pred

  # R²
  actual <- df[[lhs]]
  r2 <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  r2_label <- sprintf("R² = %.3f", r2)

  # Formula as text
  param_str <- paste(sprintf("%s=%.3f", params, opti_params$par), collapse = ", ")
  formula_label <- paste0(deparse(formula), ", ", param_str)

  # Determine predictor variable
  x_var <- setdiff(vars, params)[1]

  x_pos <- min(df[[x_var]], na.rm = TRUE)
  y_max <- max(df[[lhs]], na.rm = TRUE)

  p <- ggplot(df, aes(x = .data[[x_var]])) +
    geom_point(aes(y = .data[[lhs]])) +
    geom_line(aes(y = Predicted), color = "blue") +
    annotate("text", x = x_pos, y = y_max, label = formula_label,
      hjust = 0, vjust = 2, size = 4) +
    annotate("text", x = x_pos, y = y_max, label = r2_label,
      hjust = 0, vjust = 4, size = 4)

  p
  param_str
}

# Simulate calibration data
set.seed(42)
true_a <- 5
true_b <- 2
conc <- seq(0.1, 10, length.out = 30)
response <- true_a * conc / (true_b + conc) + rnorm(length(conc), sd = 0.2)
df <- data.frame(conc = conc, response = response)

formula <- response ~ a * conc / (b + conc)
res <- optimize(formula, df, -10000, 10000)
res

optim_res <- optimize(uptake ~ a + m*conc, CO2, -10^2, 10^2)
# TODO: offer the optim approach and nls
# describe nls as "fitting model (non least square)" and optim will be named "free optimization" formula
# Free Optimization Formula (Optim-Based)
#     "Fit any formula to your data by optimizing parameter values without statistical assumptions.
#     Ideal for calibration curves or parameter extraction where model validation is not required."
# Fitting Model (Nonlinear Least Squares)
#     "Fit a nonlinear model to your data using statistical methods.
#     Provides parameter estimates, confidence intervals, and residual diagnostics based on statistical assumptions."
# nls --> same assumptions as for lm
nls_res <- nls(uptake ~ a + m*conc, data = CO2, start = list(a = 1, m = 2))
lm_res <- lm(uptake ~ conc, data = CO2)

optim_res
nls_res
lm_res

summary(nls_res)
summary(lm_res)
