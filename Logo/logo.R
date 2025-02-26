library(hexSticker)
library(ggplot2)

# Idea: Penguins
# ==========================================================================
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
    height = penguin_size, show.legend = FALSE, colour = "black"
  ) +
  geom_phylopic(
    data = data[data$Species == "Gentoo", ],
    img = penguin2,
    aes(x = Species, y = Mean_Bill_Length),
    height = penguin_size, show.legend = FALSE, colour = "black"
  ) +
  geom_phylopic(
    data = data[data$Species == "Chinstrap", ],
    img = penguin3,
    aes(x = Species, y = Mean_Bill_Length),
    height = penguin_size, show.legend = FALSE
  ) +
  labs(y = "Mean Beak length [mm]", x = NULL) +
  theme_minimal() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.9, 0.9),
    axis.title = element_text(size = text_size, face = "bold"),
    legend.text = element_text(size = text_size, face = "bold"),
    legend.title = element_text(size = text_size, face = "bold"),
    axis.text = element_blank()
  ) +
  ylim(y_min, y_max)

create_sticker <- function(p) {
  sticker <- sticker(
    subplot = p,
    package = "Biostats",
    p_color = "black",
    s_x = 0.95, s_y = 1,
    s_width = 1.4, s_height = 1.4,
    p_size = 16,
    p_y = 1.5,
    h_fill = "#76acb0",
    h_color = "black",
    filename = "BiostatsLogo.svg"
  )
  dev.off() # Dont know what is printed here but it is anying
  ggsave("BiostatsLogo.svg",
    sticker,
    width = 4, height = 4,
    dpi = 600, bg = "white"
  )
}
create_sticker(p)
system("open BiostatsLogo.svg")

# Idea: Overlapping density plots
# ==========================================================================
set.seed(123)
df <- data.frame(y1 = rnorm(1000), y2 = rnorm(1000, mean = 2))

p <- ggplot() +
  geom_density(
    data = df,
    aes(x = y2), # Plot the white curve first
    fill = "white",
    color = "black",
    alpha = 0.9, linewidth = 1.2
  ) +
  geom_density(
    data = df,
    aes(x = y1), # Then plot the black curve on top
    fill = "black",
    alpha = 0.6,
    color = "black",
    linewidth = 1.2
  ) +
  theme_void()
create_sticker <- function(p) {
  sticker <- sticker(
    subplot = p,
    package = "Biostats",
    p_color = "black",
    s_x = 1, s_y = 0.9,
    s_width = 1.4, s_height = 0.9,
    p_size = 18,
    p_y = 1.5, p_x = 1.05,
    h_fill = "#76acb0",
    h_color = "black",
    filename = "BiostatsLogo2.svg"
  )
  dev.off() # Dont know what is printed here but it is anoying
  ggsave("BiostatsLogo2.svg",
    sticker,
    width = 4, height = 4,
    dpi = 600, bg = "white"
  )
}
create_sticker(p)
system("open BiostatsLogo2.svg")
