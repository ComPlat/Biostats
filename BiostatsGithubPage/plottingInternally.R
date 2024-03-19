	calcParams <- function(df, formula, method) {
		if (method == "lm") {
			model <- lm(formula, data = df)
			r_squared <- summary(model)$r.squared
			anova_table <- anova(model)
			f_value <- anova_table$`F value`[1]
			coefficients <- coef(model)
			equation <- paste("Y =", round(coefficients[1], 2), "+", 
	                  round(coefficients[2], 2), "* X")
			p_value <- summary(model)$coefficients[ ,4] 
			p_value <- paste(p_value, collapse = " ")
			n <- nrow(df)
			annotations <- paste("R-squared:", round(r_squared, 2),
	                     "F-value:", round(f_value, 2), "\n",
	                     "Equation:", equation,
	                     "Sample Size (n):", n, "\n",
	                     "p-values Intercept & x:", p_value )
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
			return(p)
		}
	}

	ReCreatePlot <- function(df, plotMethod) {
		p <- NULL
		if(plotMethod == "box") {
			p <- ggplot(data = df, aes(x = x, y = y, group = x)) +
				geom_boxplot()
		} else if(plotMethod == "dot") {
			p <- ggplot(data = df, aes(x = x, y = y)) +
				geom_point()
		} else if(plotMethod == "line") {
			p <- ggplot(data = df, aes(x = x, y = y)) +
				geom_line()
		}
		return(p)
	}

	annotatePlot <- function(p, method, plotMethod, level = 2, k = 5) {
		pB <- ggplot_build(p) # issue: otherwise data is empty
		df <- pB$data[[1]]	
		# https://stackoverflow.com/questions/40854225/how-to-identify-the-function-used-by-geom-smooth
		formula <- p$layers[[level]]$stat$setup_params(df, p$layers[[level]]$stat_params)$formula
		df$interaction <- interaction(df$PANEL, df$group)
		results <- lapply(unique(df$interaction), function(x) {
			sub <- df[df$interaction == x, ]
			calcParams(sub, formula, method)
		})
		df <- Reduce(rbind, results)
		names(df) <- ifelse(names(df) == "PANEL", "Panel", names(df))
		p <- ReCreatePlot(df, plotMethod)
		if(method != "gam") { 
			p <- p + geom_smooth(method = method) +
						facet_wrap(.~ Panel)
			return(p + geom_text(aes(x = xPos, y = yPos, label = annotation), size = 4))
		} else {
			p <- p + geom_smooth(method = method, formula = y ~ s(x, bs = "cs", k = k)) +
							facet_wrap(.~ Panel)
			return(p + geom_text(aes(x = xPos, y = yPos, label = annotation), size = 4))
		}
	}

	addFacet <- function(p, facetVar, facetMode) {
    	if(facetMode == "facet_wrap") {
    		p <- p + facet_wrap(.~ .data[[facetVar]], scales = "free")
    	} 
    	#else if(facetMode == "facet_grid") {
    		#return(p + facet_grid(.~ .data[[facetVar]], scales = "free") )
    	#}
	}

	BoxplotFct <- function(df, x, y, xLabel, yLabel,
										fillVar, legendTitleFill, fillTheme, 
										colourVar, legendTitleColour,
										colourTheme, facetMode, facetVar) {
		aes <- aes(x = .data[[x]], y = .data[[y]])
		aesColour = NULL
		aesFill = NULL
		p <- NULL
		if (!missing(colourVar)) {
			aesColour <- aes(colour = .data[[colourVar]])
		}
		if (!missing(fillVar)) {
			aesFill <- aes(fill = .data[[fillVar]])
		}
		p <- ggplot() +
					geom_boxplot(data = df,
											 aes(!!!aes, !!!aesColour, !!!aesFill,
													group = interaction(.data[[x]],
																		 !!!aesColour, !!!aesFill) ) )	
		if (!missing(xLabel)) p <- p + xlab(xLabel)
		if (!missing(yLabel)) p <- p + ylab(yLabel)
		if (!missing(legendTitleFill)) p <- p + guides(fill = guide_legend(title = legendTitleFill))
		if (!missing(legendTitleColour)) p <- p + guides(colour = guide_legend(title = legendTitleColour))
		if (!missing(fillTheme)) p <- p + scale_fill_brewer(palette = fillTheme)	
		if (!missing(colourTheme)) p <- p + scale_color_brewer(palette = colourTheme)	
    if (missing(facetVar) | missing(facetMode)) {
    	return(p)
    } else {
    	p <- addFacet(p, facetVar, facetMode)
    }
		return(p)
	}

BoxplotFct(CO2, "conc", "uptake", xLabel = "bla", yLabel = "uptake2",
					 fillVar = "Treatment", legendTitleFill = "Treament fill", fillTheme = "PuOr",
					 colourVar = "Type", legendTitleColour = "bla", colourTheme = "hue",
					 "facet_wrap", "Type")
#BoxplotFct(CO2, "conc", "uptake", colourVar = "Type")
#BoxplotFct(CO2, "conc", "uptake", colourVar = "Type")
#PlFct("dot", CO2, "conc", "uptake", colourVar = "Type", fillVar = "Treatment")
	#method <- "lm"
	#p <- ggplot(data = CO2, aes(x = conc, y = uptake)) +
	#	geom_point(size = 0) +
	#	geom_boxplot(data = CO2, aes(x = conc, y = uptake, group = conc)) +
	#	geom_smooth(method = method) 
	#p
	#annotatePlot(p, method, "box", 3)

	# y
	# x
	# type of x: numeric or factor
	# x label
	# y label
	# fitting method: none, lm, glm, gam & loess
	# fill variable
	# legend title fill
	# colour variable
	# legend title colour
	# colour theme
	# fill theme
	# facet mode: none, wrap, grid
	# split plot by