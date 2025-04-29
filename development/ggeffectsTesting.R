source("./bs/R/SummarisingModel.R")
source("./bs/R/StorageClasses.R")
library(ggplot2)
library(cowplot)

compare <- function(data, formula) {
  formula_class <- new("GeneralisedLinearFormula", formula = formula, family = "Gamma", link_fct = "identity")
  p1 <- plot_pred(data, formula_class)
  model <- lm(formula, data)
  f_split <- split_formula(formula)
  predictors <- vars_rhs(f_split$right_site)
  p2 <- ggeffects::predict_response(model, terms = predictors) |> plot()
  plot_grid(p1, p2)
}
data <- CO2
data$conc_perturbed <- runif(nrow(data))*200
data$bla <- rnorm(nrow(data))*20

f <- uptake ~ bla
compare(data, f)

f <- uptake ~ 0 + conc + I(bla)
compare(data, f)
f <- uptake ~ Type # Works
compare(data, f)
f <- uptake ~ conc # Works
compare(data, f)
f <- uptake ~ conc*Type # Works
compare(data, f)
f <- uptake ~ Type * conc # Works
compare(data, f)
f <- uptake ~ Type * Treatment # Works
compare(data, f)
f <- uptake ~ conc * conc_perturbed # Works
compare(data, f)
f <- uptake ~ conc * Type * Treatment # Works
compare(data, f)
f <- uptake ~ conc * conc_perturbed * Treatment # Works
compare(data, f)
f <- uptake ~ conc_perturbed * Treatment * conc * Type # Works
compare(data, f)
f <- uptake ~ conc_perturbed * Treatment * conc * Type * bla
plot_pred(data, f)

data <- mtcars
data$cyl <- as.factor(data$cyl)
f <- mpg ~ hp * cyl  # Interaction: numeric * factor
compare(data, f)
f <- mpg ~ hp * wt * cyl * gear
compare(data, f)
f <- mpg ~ hp * wt * cyl * gear * carb * disp
compare(data, f)  # Should trim to 4 predictors and warn
f <- mpg ~ hp * wt * disp * qsec
compare(data, f)

