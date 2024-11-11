addFacet <- function(p, facetVar, facetMode, facetScales) {
  if (facetMode == "facet_wrap") {
    return(p + facet_wrap(. ~ .data[[facetVar]], scales = facetScales))
  } else if (facetMode == "facet_grid") {
    return(p + facet_grid(. ~ .data[[facetVar]], scales = facetScales))
  }
}

addInterval <- function(p, df, xCol, yCol, xMin, xMax, yMin, yMax) {
  x <- df[ , xCol]
  y <- df[ , yCol]
  if (is.numeric(x)) {
    p <- p + scale_x_continuous(limits = c(xMin, xMax))
  } else {
    choices <- unique(x)
    xStart <- which(xMin == choices)
    xEnd <- which(xMax == choices)
    p <- p + scale_x_discrete(limits = choices[xStart:xEnd])
  }
  if (is.numeric(y)) {
    p <- p + scale_y_continuous(limits = c(yMin, yMax))
  } else {
    choices <- unique(y)
    yStart <- which(yMin == choices)
    yEnd <- which(yMax == choices)
    p <- p + scale_y_discrete(limits = choices[yStart:yEnd])
  }
  return(p)
}

annotateDF <- function(p, method, level = 2) {
  pB <- ggplot_build(p)
  df <- pB$data[[1]]
  if (length(unique(df$PANEL)) > 1) {
    l <- pB$layout$layout
    l <- data.frame(PANEL = l$PANEL, names = l$`<unknown>`)
    df$PANEL <- l[match(df$PANEL, l$PANEL), 2]
  }
  # https://stackoverflow.com/questions/40854225/how-to-identify-the-function-used-by-geom-smooth
  formula <- p$layers[[level]]$stat$setup_params(
    df,
    p$layers[[level]]$stat_params
  )$formula
  df$interaction <- interaction(df$PANEL, df$group)

  results <- lapply(unique(df$interaction), function(x) {
    sub <- df[df$interaction == x, ]
    calcParams(sub, formula, method)
  })
  df <- Reduce(rbind, results)
  return(df)
}

calcParams <- function(df, formula, method) {
  check_ast(formula, colnames(df))
  if (method == "lm") {
    model <- lm(formula, data = df)
    r_squared <- summary(model)$r.squared
    anova_table <- anova(model)
    f_value <- anova_table$`F value`[1]
    coefficients <- coef(model)
    equation <- paste(
      "Y =", round(coefficients[1], 2), "+",
      round(coefficients[2], 2), "* X"
    )
    p_value <- summary(model)$coefficients[, 4]
    p_value <- round(p_value, 6)
    p_value <- paste(p_value, collapse = ", ")
    n <- nrow(df)
    annotations <- paste(
      "R-squared:", round(r_squared, 2),
      "F-value:", round(f_value, 2), "\n",
      "Equation:", equation, "\n",
      "Sample Size (n):", n, "\n",
      "p-values Intercept & x:", p_value
    )
    df$annotation <- annotations
    df$xPos <- mean(df$x)
    df$yPos <- max(df$y)
    df$xPos <- mean(df$x)
    return(df)
  } else if (method == "glm") {
    model <- glm(formula, data = df)
    r_squared <- with(summary(model), 1 - deviance / null.deviance)
    coefficients <- coef(model)
    n <- nrow(df)
    equation <- paste(
      "Y =", round(coefficients[1], 2), "+",
      round(coefficients[2], 2), "* X"
    )
    p_value <- summary(model)$coefficients[2, 4]
    annotations <- paste(
      "R-squared:", round(r_squared, 2),
      "Sample Size (n):", n, "\n",
      "Equation:", equation, "\n",
      "p-value:", round(p_value, 6)
    )
    df$annotation <- annotations
    df$xPos <- mean(df$x)
    df$yPos <- max(df$y)
    return(df)
  } else if (method == "gam") {
    model <- gam(formula, data = df)
    r_squared <- summary(model)$r.sq
    f_value <- summary(model)$p.t
    coefficients <- coef(model)
    n <- nrow(df)
    equation <- paste(
      "Y =", round(coefficients[1], 2), "+",
      round(coefficients[2], 2), "* X"
    )
    p_value <- summary(model)$p.pv
    annotations <- paste(
      "R-squared:", round(r_squared, 2),
      "F-value:", round(f_value, 2), "\n",
      "Equation:", equation,
      "Sample Size (n):", n, "\n",
      "p-value:", round(p_value, 6)
    )
    df$annotation <- annotations
    df$xPos <- mean(df$x)
    df$yPos <- max(df$y)
    return(df)
  } else if (method == "loess") {
    model <- loess(formula, data = df)
    fitted_values <- predict(model)
    r_squared <- cor(df$y, fitted_values)^2
    n <- nrow(df)
    annotations <- paste(
      "R-squared:", round(r_squared, 2),
      "Sample Size (n):", n
    )
    df$annotation <- annotations
    df$xPos <- mean(df$x)
    df$yPos <- max(df$y)
    return(df)
  }
}

DotplotFct <- function(df, x, y, xLabel, yLabel,
                       fitMethod,
                       colourVar, legendTitleColour,
                       colourTheme, facetMode, facetVar, facetScales,
                       k = 10,
                       xMin, xMax, yMin, yMax) {
  # create plot
  # ==========================================
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL

  if (colourVar != "") {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- aes(colour = .data[[colourVar]])
    }
  }
  if (colourVar == "") {
    p <- ggplot(
      data = df,
      aes(!!!aes)
    ) +
      geom_point(position = position_dodge(width = 0.5))
  } else {
    p <- ggplot(
      data = df,
      aes(!!!aes, !!!aesColour)
    ) +
      geom_point(position = position_dodge(width = 0.5))
  }

  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  p <- addInterval(p, df, x, y, xMin, xMax, yMin, yMax)
  if (colourVar != "") {
    p <- p + guides(colour = guide_legend(title = legendTitleColour))
    p <- p + scale_color_brewer(palette = colourTheme)
  }

  if (facetMode != "none") {
    p <- addFacet(p, facetVar, facetMode)
  }

  if (fitMethod == "none" || fitMethod == "") {
    return(p)
  }

  # fit data
  # ==========================================
  if (fitMethod == "gam") {
    p <- p + geom_smooth(
      method = fitMethod,
      formula = y ~ s(x, bs = "cs", k = k)
    )
  } else {
    p <- p + geom_smooth(method = fitMethod)
  }

  # extract information from fit
  # ==========================================
  df_original <- df
  df <- annotateDF(p, fitMethod)
  names(df) <- ifelse(names(df) == "PANEL", "Panel", names(df))

  # TODO: this is a hack. Find a better way.
  if (colourVar != "") {
    df$colour_groups <- df_original[, colourVar][match(
      df$group,
      as.integer(factor(df_original[, colourVar]))
    )]
  }
  # Add annotations to plot
  # ==========================================
  aes <- aes(x = .data[["x"]], y = .data[["y"]])
  if (colourVar != "") {
    aesColour <- aes(colour = .data[["colour_groups"]])
  }
  if (fitMethod == "gam") {
    p <- ggplot(data = df, aes(!!!aes, !!!aesColour)) +
      geom_point(data = df, aes(shape = annotation), position = position_dodge(width = 0.5)) +
      geom_smooth(
        method = fitMethod,
        formula = y ~ s(x, bs = "cs", k = k)
      ) +
      theme(legend.position = "bottom")
  } else {
    p <- ggplot(data = df, aes(!!!aes, !!!aesColour)) +
      geom_point(data = df, aes(shape = annotation), position = position_dodge(width = 0.5)) +
      geom_smooth(method = fitMethod) +
      theme(legend.position = "bottom")
  }

  # Add labels
  # ==========================================
  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  if (length(unique(df$colour)) >= 2) {
    p <- p + guides(colour = guide_legend(title = legendTitleColour))
    p <- p + scale_color_brewer(palette = colourTheme)
  }
  if (facetMode != "none") {
    p <- addFacet(p, "Panel", facetMode, facetScales)
  }

  return(p)
}

BoxplotFct <- function(df, x, y, xLabel, yLabel,
                       fillVar, legendTitleFill, fillTheme,
                       colourVar, legendTitleColour,
                       colourTheme, facetMode, facetVar, facetScales,
                       xMin, xMax, yMin, yMax) {
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  aesFill <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- aes(colour = .data[[colourVar]])
    }
  }
  if (fillVar == "") {
    aesFill <- aes()
  } else {
    if (is.numeric(df[[fillVar]])) {
      aesFill <- aes(fill = factor(.data[[fillVar]]))
    } else {
      aesFill <- aes(fill = .data[[fillVar]])
    }
  }
  p <- ggplot() +
    geom_boxplot(
      data = df,
      aes(!!!aes, !!!aesColour, !!!aesFill,
        group = interaction(
          .data[[x]],
          !!!aesColour, !!!aesFill
        )
      )
    )
  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  p <- p + guides(fill = guide_legend(title = legendTitleFill))
  p <- p + guides(colour = guide_legend(title = legendTitleColour))
  p <- p + scale_fill_brewer(palette = fillTheme)
  p <- p + scale_color_brewer(palette = colourTheme)
  p <- addInterval(p, df, x, y, xMin, xMax, yMin, yMax)
  if (facetMode != "none") {
    p <- addFacet(p, facetVar, facetMode, facetScales)
  }
  return(p)
}

LineplotFct <- function(df, x, y, xLabel, yLabel,
                        colourVar, legendTitleColour,
                        colourTheme, facetMode, facetVar, facetScales,
                        xMin, xMax, yMin, yMax) {
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- aes(colour = .data[[colourVar]])
    }
  }
  p <- ggplot() +
    geom_line(
      data = df,
      aes(!!!aes, !!!aesColour,
        group = interaction(
          .data[[x]],
          !!!aesColour
        )
      )
    )
  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  p <- p + guides(colour = guide_legend(title = legendTitleColour))
  p <- p + scale_color_brewer(palette = colourTheme)
  p <- addInterval(p, df, x, y, xMin, xMax, yMin, yMax)
  if (facetMode != "none") {
    p <- addFacet(p, facetVar, facetMode, facetScales)
  }
  return(p)
}
