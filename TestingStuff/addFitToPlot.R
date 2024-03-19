library(ggplot2)
library(broom)
library(mgcv)
method <- "glm"
p <- ggplot(data = CO2, aes(x = conc, y = uptake, colour = Treatment)) +
	geom_point() +
	geom_smooth(method = method) +
	#geom_smooth(method = method, formula = y ~ s(x, bs = "cs", k = 5)) +
	facet_wrap(.~ Type)
p
# https://stackoverflow.com/questions/40854225/how-to-identify-the-function-used-by-geom-smooth
getInfo <- function(plot, layer = 2) {
  layerData <- plot$layers[[layer]]$layer_data(plot$data)
  layout <- ggplot2:::create_layout(plot$facet, plot$coordinates)
  data <- layout$setup(list(layerData), plot$data, plot$plot_env)
  data[[1]] <- plot$layers[[layer]]$compute_aesthetics(data[[1]], plot)
  scales <- plot$scales
  data[[1]] <- ggplot2:::scales_transform_df(scales = scales, df = data[[1]])
  layout$train_position(data, scales$get_scales("x"), scales$get_scales("y"))
  data <- layout$map_position(data)[[1]]
  statParams <- suppressMessages(
    plot$layers[[layer]]$stat$setup_params(data = data, 
                                           params = plot$layers[[layer]]$stat_params)
    )
  if(identical(statParams$method, mgcv::gam)) statParams$method <- "gam"
  return(list(statParams, data))
}

res <- getInfo(p, 2)
resModel <- res[[1]]
formula <- resModel$formula
method <- resModel$method
df <- res[[2]]
df$interaction <- interaction(df$PANEL, df$group)

calcParams <- function(df, formula, method) {
	if (method == "lm") {
		model <- lm(formula, data = df)
		r_squared <- summary(model)$r.squared
		anova_table <- anova(model)
		f_value <- anova_table$`F value`[1]
		coefficients <- coef(model)
		equation <- paste("Y =", round(coefficients[1], 2), "+", 
                  round(coefficients[2], 2), "* X")
		p_value <- summary(model)$coefficients[2,4] 
		n <- nrow(df)
		annotations <- paste("R-squared:", round(r_squared, 2),
                     "F-value:", round(f_value, 2), "\n",
                     "Equation:", equation,
                     "Sample Size (n):", n, "\n",
                     "p-value:", round(p_value, 8) )
		df$annotation <- annotations
		df$xPos <- mean(df$x)
		df$yPos <- max(df$y)
		return(df)
	} else if(method == "glm") {
		model <- glm(formula, data = df)
		r_squared <- with(summary(model), 1 - deviance/null.deviance)
		coefficients <- coef(model)
		n <- nrow(df)
		equation <- paste("Y =", round(coefficients[1], 2), "+", 
                  round(coefficients[2], 2), "* X")
		p_value <- summary(model)$coefficients[2,4] 
		annotations <- paste("R-squared:", round(r_squared, 2),
                     "Equation:", equation,
                     "Sample Size (n):", n, "\n",
                     "p-value:", round(p_value, 16) )
		df$annotation <- annotations
		df$xPos <- mean(df$x)
		df$yPos <- max(df$y)
		return(df)
	} else if(method == "gam") {
		model <- gam(formula, data = df)
		print(formula)
		r_squared <- summary(model)$r.sq
		f_value <- summary(model)$p.t
		coefficients <- coef(model)
		n <- nrow(df)
		equation <- paste("Y =", round(coefficients[1], 2), "+", 
                  round(coefficients[2], 2), "* X")
		p_value <- summary(model)$p.pv
		annotations <- paste("R-squared:", round(r_squared, 2),
                     "F-value:", round(f_value, 2), "\n",
                     "Equation:", equation,
                     "Sample Size (n):", n, "\n",
                     "p-value:", round(p_value, 16) )
		df$annotation <- annotations
		df$xPos <- mean(df$x)
		df$yPos <- max(df$y)
		return(df)
	} else if(method == "loess") {
		
	}
}

results <- lapply(unique(df$interaction), function(x) {
	sub <- df[df$interaction == x, ]
	calcParams(sub, formula, method)
})
df <- Reduce(rbind, results)
names(df) <- ifelse(names(df) == "PANEL", "Panel", names(df))
p <- ggplot(data = df, aes(x = x, y = y, colour = colour)) +
	geom_point() +
	#geom_smooth(method = method, formula = y ~ s(x, bs = "cs", k = 5)) +
	geom_smooth(method = method) +
	facet_wrap(.~ Panel)
p
p + geom_text(aes(x = xPos, y = yPos, label = annotation), size = 3)