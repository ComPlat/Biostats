source("../BiostatsGithubPage/check_ast.R")
library(ggplot2)

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
  stopifnot(get_ast(formula) != "Error")
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
    p_value <- paste(p_value, collapse = " ")
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
      "p-value:", round(p_value, 16)
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
      "p-value:", round(p_value, 16)
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

addFacet <- function(p, facetVar, facetMode) {
  if (facetMode == "facet_wrap") {
    return(p + facet_wrap(. ~ .data[[facetVar]], scales = "free"))
  } else if (facetMode == "facet_grid") {
    return(p + facet_grid(. ~ .data[[facetVar]], scales = "free"))
  }
}

DotplotFct <- function(df, x, y, xLabel, yLabel,
                       fitMethod,
                       colourVar, legendTitleColour,
                       colourTheme, facetMode, facetVar, k = 10) {
  # create plot
  # ==========================================
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL

  if (colourVar != "") {
    aesColour <- aes(colour = .data[[colourVar]])
  }
  if (colourVar == "") {
    p <- ggplot(
      data = df,
      aes(!!!aes)
    ) +
      geom_point()
  } else {
    p <- ggplot(
      data = df,
      aes(!!!aes, !!!aesColour)
    ) +
      geom_point()
  }

  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)

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
  df$colour_groups <- df_original[, colourVar][match(
    df$group,
    as.integer(factor(df_original[, colourVar]))
  )]

  # Add annotations to plot
  # ==========================================
  aes <- aes(x = .data[["x"]], y = .data[["y"]])
  if (colourVar != "") {
    aesColour <- aes(colour = .data[["colour_groups"]])
  }
  if (fitMethod == "gam") {
    p <- ggplot(data = df, aes(!!!aes, !!!aesColour)) +
      geom_point() +
      geom_smooth(
        method = fitMethod,
        formula = y ~ s(x, bs = "cs", k = k)
      ) +
      geom_text(
        aes(
          x = xPos, y = yPos,
          label = annotation
        ),
        size = 2.5,
        show.legend = FALSE, position = position_dodge(width = .9)
      )
  } else {
    p <- ggplot(data = df, aes(!!!aes, !!!aesColour)) +
      geom_point() +
      geom_smooth(method = fitMethod) +
      geom_text(
        aes(
          x = xPos, y = yPos,
          label = annotation
        ),
        size = 2.5,
        show.legend = FALSE, position = position_dodge(width = .9)
      )
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
    p <- addFacet(p, "Panel", facetMode)
  }

  return(p)
}

df <- CO2
df$Type_Treatment <- paste(
  df$Type, df$Treatment,
  sep = "_"
)

DotplotFct(
  df = df,
  x = "conc",
  y = "uptake",
  xLabel = "",
  yLabel = "",
  fitMethod = "glm",
  colourVar = "Type_Treatment",
  legendTitleColour = "Type & Treatment",
  colourTheme = "Set1",
  facetMode = "facet_wrap",
  facetVar = "Treatment"
)
