library(hexSticker)
library(ggplot2)
library(palmerpenguins)
library(rphylopic)
df <- palmerpenguins::penguins |> as.data.frame()

penguin1 <- pick_phylopic("Pygoscelis", n = 3, view = 3) # click 2
penguin2 <- pick_phylopic("Pygoscelis", n = 3, view = 3) # click 3
penguin3 <- pick_phylopic("Pygoscelis", n = 9, view = 9) # click 4
pen1 <- rotate_phylopic(img = penguin1, angle = 15)
pen2 <- rotate_phylopic(img = penguin2, angle = 15)
pen3 <- rotate_phylopic(img = penguin3, angle = 15)

y_min <- min(df$bill_length_mm, na.rm = TRUE) * 0.9
y_max <- max(df$bill_length_mm, na.rm = TRUE) * 1.05

data <- data.frame(
  Species = c("Adelie", "Gentoo", "Chinstrap"),
  Mean_Bill_Length = sapply(unique(df$species), function(x) {
    temp <- df[df$species == x, ]
    mean(temp$bill_length_mm, na.rm = TRUE)
  })
)
penguin_size <- 12
text_size <- 10
p <- ggplot() +
  geom_phylopic(
    data = data[data$Species == "Adelie", ],
    img = penguin1,
    aes(x = Species, y = Mean_Bill_Length),
    height = penguin_size, show.legend = FALSE, colour = "orange"
  ) +
  geom_phylopic(
    data = data[data$Species == "Gentoo", ],
    img = penguin2,
    aes(x = Species, y = Mean_Bill_Length),
    height = penguin_size, show.legend = FALSE, colour = "darkblue"
  ) +
  geom_phylopic(
    data = data[data$Species == "Chinstrap", ],
    img = penguin3,
    aes(x = Species, y = Mean_Bill_Length),
    height = penguin_size, show.legend = FALSE
  ) +
  labs(y = "Mean Bill length [mm]", x = "Species") +
  theme_minimal() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.9, 0.9),
    axis.title = element_text(size = text_size, face = "bold"),
    axis.text = element_text(size = text_size, face = "bold"),
    legend.text = element_text(size = text_size, face = "bold"),
    legend.title = element_text(size = text_size, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(y_min, y_max)

sticker <- sticker(
  subplot = p,
  package = "Biostats",
  p_color = "black",
  s_x = 1, s_y = 1,
  s_width = 1.4, s_height = 1.4,
  p_size = 14,
  h_fill = "#76acb0",
  h_color = "black",
  filename = "BiostatsLogo.svg"
)
ggsave("BiostatsLogo.svg", sticker, width = 4, height = 4, bg = "white")
