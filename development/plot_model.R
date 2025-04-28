source("./bs/R/SummarisingModel.R")
library(ggplot2)
library(cowplot)

data <- CO2
data$bla <- data$conc * runif(nrow(data), min(data$conc), max(data$conc))
f <- uptake ~ conc
plot_model(data, f, "dot")
plot_model(data, f, "box")
plot_model(data, f, "line")
f <- uptake ~ conc * Type
plot_model(data, f, "dot")
plot_model(data, f, "box")
f <- uptake ~ Type * Treatment
plot_model(data, f, "box")
plot_model(data, f, "dot")
f <- uptake ~ conc * Type * Treatment
plot_model(data, f, "box")
plot_model(data, f, "dot")
f <- uptake ~ conc * bla* Type * Treatment
plot_model(data, f, "box")
plot_model(data, f, "dot")
