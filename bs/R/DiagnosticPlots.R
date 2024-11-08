CooksDistance <- function(df, formula) {
  # https://rpubs.com/DragonflyStats/Cooks-Distance
  model <- lm(formula, data = df)
  cd <- cooks.distance(model)
  cd <- data.frame(CooksDistance = cd,
    Index = 1:length(cd))
  n <- nrow(df)
  # NOTE: number of predictors -1 (remove intercept via -1)
  k <- length(coef(model)) - 1 
  cutoff <- 4 / (n - k - 1)
  ggplot(data = cd, aes(Index, CooksDistance)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = cutoff,
      linetype = "dashed",
      color = "black",
      linewidth = 0.25
    ) +
    geom_text(data = cd[cd$CooksDistance >= cutoff, ],
      aes(label = Index),
      vjust = -1,
      color = "black",
      size = 3)
}

diagnosticPlots <- function(df, formula) {
  model <- lm(formula, data = df)
  resids <- residuals(model)
  fitted <- fitted(model)

  # Identify influential points
  p <- length(coef(model))
  n <- length(resids)
  leverage <- hatvalues(model)
  cooks_dist <- cooks.distance(model)
  high_leverage_threshold <- (2 * (p + 1)) / n
  high_cooks_threshold <- 4 / n
  influential_points <- which(
    leverage > high_leverage_threshold | cooks_dist > high_cooks_threshold
  )

  # resids vs fitted
  line_data <- lowess(fitted, resids) |> as.data.frame()
  resids_fitted_df <- data.frame(fitted = fitted, residuals = resids, index = 1:n)
  resids_vs_fitted <- ggplot(
    data = resids_fitted_df,
    aes(x = fitted, y = residuals)
  ) +
    geom_point() +
    geom_hline(yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = 0.25
    ) +
    geom_line(
      data = line_data,
      aes(x = x, y = y)
    ) +
    geom_text(data = resids_fitted_df[influential_points, ],
      aes(label = index),
      vjust = -1,
      color = "black",
      size = 3) +
    labs(
      y = "Residuals", x = "Fitted values",
      title = "Residuals vs Fitted values",
      caption = "In this plot, residuals should scatter randomly around zero
with consistent spread across fitted values for homoscedasticity.
Patterns like fanning or funneling indicate heteroscedasticity, where residual
variance changes with fitted values, violating the homoscedasticity assumption."
    ) +
    theme(plot.caption = element_text(size = 12, hjust = 0))

  # qq norm plot
  standardized_resids <- resids / sd(resids)
  sqrt_resids <- sqrt(abs(standardized_resids))
  ordered_resids <- sort(standardized_resids)
  theoretical_quantiles <- qnorm((1:n) / (n + 1))
  resids_quantiles_df <- data.frame(
    quantiles = theoretical_quantiles, residuals = ordered_resids,
    index = 1:n
  )
  slope <- sd(ordered_resids)
  intercept <- mean(ordered_resids)
  resids_vs_quantiles <- ggplot(
    data = resids_quantiles_df,
    aes(x = quantiles, y = residuals)
  ) +
    geom_point() +
    geom_abline(
      aes(
        slope = slope,
        intercept = intercept
      )
    ) +
    geom_text(data = resids_quantiles_df[influential_points, ],
      aes(label = index),
      vjust = -1,
      color = "black",
      size = 3) +
    labs(
      y = "Standardized residuals", x = "Theoretical Quantiles",
      title = "Q-Q Residuals"
    )

  # Manual Scale-Location Plot
  line_data <- lowess(fitted, sqrt_resids) |> as.data.frame()
  sqrt_resids_fitted_df <- data.frame(
    fitted = fitted, residuals = sqrt_resids,
    index = 1:n
  )
  sqrt_resids_vs_fitted <- ggplot(
    data = sqrt_resids_fitted_df,
    aes(x = fitted, y = residuals)
  ) +
    geom_point() +
    geom_line(
      data = line_data,
      aes(x = x, y = y),
      colour = "black"
    ) +
    geom_text(data = sqrt_resids_fitted_df[influential_points, ],
      aes(label = index),
      vjust = -1,
      color = "black",
      size = 3) +
    labs(
      y = expression(sqrt("Standardized residuals")),
      x = "Fitted values",
      title = "Scale-Location"
    )

  # Residuals vs Leverage
  line_data <- lowess(leverage, standardized_resids) |> as.data.frame()
  residuals_leverage_df <- data.frame(
    residuals = standardized_resids, leverage = leverage,
    index = 1:n
  )
  residuals_vs_leverage <- ggplot(
    data = residuals_leverage_df,
    aes(x = leverage, y = residuals)
  ) +
    geom_point() +
    geom_line(
      data = line_data,
      aes(x = x, y = y)
    ) +
    geom_hline(yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = 0.25
    ) +
    geom_text(data = residuals_leverage_df[influential_points, ],
      aes(label = index),
      vjust = -1,
      color = "black",
      size = 3) +
    labs(
      y = "Standardized residuals", x = "Leverage",
      title = "Residuals vs Leverage"
    )

  plot_grid(
    resids_vs_fitted,
    resids_vs_quantiles,
    sqrt_resids_vs_fitted,
    residuals_vs_leverage,
    CooksDistance(df, formula)
  )
}


