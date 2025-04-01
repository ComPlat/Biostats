library(ggplot2)

# Plot using formula
pf <- function(df, formula) {
  f <- as.character(formula)
  y <- f[2]
  dep <- all.vars(str2lang(f[3]))
  x <- dep[1]
  dep <- dep[-1]
  facet <- dep[1]
  dep <- dep[-1]
  interaction <- interaction(df[, dep])
  df$interaction <- interaction
  aes <- aes(x = factor(
    x = .data[[x]]),
    y = .data[[y]],
    fill = .data[["interaction"]]
  )
  p <- ggplot(
    data = df,
    aes(!!!aes)
  ) +
    geom_boxplot() +
    facet_wrap(
      ~ .data[[facet]],
      scales = "free"
    )
  return(p)
}

pf(
  CO2,
  formula(uptake ~ conc + Treatment * Type)
)
