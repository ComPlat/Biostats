dnorm(0.25, mean = 0, sd = 1)
dnorm(0.5, mean = 0, sd = 1)
dnorm(0.005, mean = 0, sd = 1)
dnorm(5, mean = 0, sd = 1)
dnorm(0.01, mean = 0, sd = 1)

mean <- 0
sd <- 1
set.seed(123) # For reproducibility
samples <- rnorm(1000, mean, sd)
hist(samples,
  breaks = 30, probability = TRUE, col = "lightgray",
  main = "Histogram of Random Samples with PDF Overlay",
  xlab = "x", ylab = "Density", ylim = c(0, 1.25)
)

x <- seq(-4, 4, length.out = 100)
lines(x, dnorm(x, mean, sd), col = "blue", lwd = 2)

x <- seq(-4, 4, length.out = 100)
lines(x, pnorm(x, mean, sd), type = "l", lwd = 2, col = "red")

lines(density(samples), col = "green", lwd = 2)

legend("topright",
  legend = c("PDF (dnorm)", "CDF (pnorm)", "Empirical Density", "Histogram of Samples"),
  col = c("blue", "red", "green", "lightgray"), lwd = 2, fill = c(NA, NA, NA, "lightgray")
)
