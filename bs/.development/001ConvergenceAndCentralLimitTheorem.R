# Mean
mean_exp <- function(n) {
  x <- rexp(n, 1)
  return(mean(x))
}
n <- seq(1, 10^4, by = 10)
ms <- lapply(n, mean_exp)
plot(n, ms)

# Variance
var_exp <- function(n) {
  x <- rexp(n, 1)
  return(sd(x))
}
n <- seq(1, 10^4, by = 10)
vs <- lapply(n, var_exp)
plot(n, vs)


# Central Limit Theorem Simulation
sample_means <- function(sample_size, n_samples) {
  replicate(n_samples, mean(rexp(sample_size, rate = 10)))
}
sample_sizes <- c(5, 30, 100, 1000, 10000, 10^5)
n_samples <- 10000

clt_data <- lapply(sample_sizes, function(size) {
  data.frame(
    x = 1:n_samples,
    mean = sample_means(size, n_samples),
    sample_size = factor(size)
  )
})

library(ggplot2)
library(cowplot)

p_fct <- function(df) {
  ggplot(df) +
    geom_histogram(aes(x = mean, y = ..density..), bins = 50, fill = "lightblue") +
    geom_density(aes(x = mean), color = "darkred", size = 1.5)
}
plot_list <- lapply(clt_data, p_fct)

plot_grid(plotlist = plot_list, ncol = 2, labels = "AUTO")
