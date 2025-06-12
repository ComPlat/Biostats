library(ggplot2)

files <- list.files("./bs/R", full.names = TRUE)
trash <- lapply(files, source)

# Simulate calibration data
set.seed(42)
true_a <- 5
true_b <- 2
conc1 <- seq(0.1, 10, length.out = 30)
conc2 <- seq(0.1, 10, length.out = 30)
response <- true_a * conc1 / (true_b + conc2) + rnorm(length(conc1), sd = 0.2)
df <- data.frame(conc1 = conc1, conc2 = conc2, response = response)
formula <- response ~ a * conc1 / (b + conc2)
formula_optim <- create_formula_optim(formula, df, 0, 100, 1234)
res <- optimize(formula_optim, df)
res@parameter
p <- plot_model_optim(formula_optim, res)
p
assumptions_optim(res)


summary_model_optim(formula_optim, res)
information_criterion_optim(res)
