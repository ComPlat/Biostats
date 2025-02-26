library(hexSticker)
library(ggplot2)
library(palmerpenguins)
library(rphylopic)
df <- palmerpenguins::penguins |> as.data.frame()

penguin1 <- rphylopic::get_phylopic("86334821-42ec-4da1-bb9d-53f3d6941c77")
penguin2 <- rphylopic::get_phylopic("a4112793-e464-4d6f-8bde-8976bcca100a")
penguin3 <- rphylopic::get_phylopic("9940201e-c445-4ff9-9742-8c3bdac04643")
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
  labs(y = "Mean Beak length [mm]", x = "Species") +
  theme_minimal() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.9, 0.9),
    axis.title = element_text(size = text_size, face = "bold"),
    legend.text = element_text(size = text_size, face = "bold"),
    legend.title = element_text(size = text_size, face = "bold"),
    axis.text = element_blank()
  ) +
  ylim(y_min, y_max)

sticker <- sticker(
  subplot = p,
  package = "Biostats",
  p_color = "black",
  s_x = 0.95, s_y = 1,
  s_width = 1.4, s_height = 1.4,
  p_size = 12,
  p_y = 1.5,
  h_fill = "#76acb0",
  h_color = "black",
  filename = "BiostatsLogo.svg"
)

print(sticker)
ggsave("BiostatsLogo.svg", sticker, width = 4, height = 4, bg = "white")
