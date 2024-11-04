library(tinyplot)
library("grid")
library("ggplotify")

f <- formula(uptake ~ conc | Treatment)


p <- as.ggplot(
  expression(
    tinyplot(
      f,
      data = CO2,
      # facet = "by",
      # facet.args = list(bg = "grey90"),
      type = "boxplot",
      palette = "dark2",
      grid = TRUE,
      axes = "l",
      facet = Treatment ~ Type,
      facet.args = list(col = "white", bg = "black"),
      main = "CO2 uptake by conc and Treatment"
    )
  )
)
p


p2 <- as.ggplot(expression(plot(rnorm(10))))
p2
