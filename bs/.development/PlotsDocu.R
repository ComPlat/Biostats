library(ggplot2)
library(cowplot)
df <- CO2

p1 <- ggplot(data = df,
  aes(x = conc, y = uptake, group = conc)) +
  geom_boxplot() +
  theme(
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24)
  )

p2 <- ggplot(data = df,
  aes(x = conc, y = uptake,
    group = interaction(conc, Treatment),
    fill = Treatment)) +
  geom_boxplot() +
  theme(
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )


p3 <- ggplot(data = df,
  aes(x = conc, y = uptake,
    group = interaction(conc, Treatment),
    colour = Treatment)) +
  geom_boxplot() +
  theme(
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )

p4 <- ggplot(data = df,
  aes(x = conc, y = uptake,
    group = conc)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme(
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.text = element_text(size = 20)
  )


p <- plot_grid(p1, p2, p3, p4, labels = c("a", "b", "c", "d"), label_size = 40)

ggsave(filename = "../R/www/DocuPlot.jpg", p, width = 20, height = 20)


