library(ggplot2)
library(shinytest2)
library(tinytest)
app <- bs::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$wait_for_idle()
app$set_window_size(width = 2259, height = 1326)
app$wait_for_idle()
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "bs")
)
app$wait_for_idle()
app$set_inputs(conditionedPanels = "Visualisation")
app$wait_for_idle()
app$set_inputs(`VIS-yVar` = "uptake")
app$wait_for_idle()
app$set_inputs(`VIS-xVar` = "conc")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()

# Test basic plot
x <- "conc"
y <- "uptake"
df <- CO2
x_min <- min(df[[x]], na.rm = TRUE)
padded_min_x <- x_min * 0.5
x_max <- max(df[[x]], na.rm = TRUE)
padded_max_x <- x_max * 1.25
y_min <- min(df[[y]], na.rm = TRUE)
padded_min_y <- y_min * 0.95
y_max <- max(df[[y]], na.rm = TRUE)
padded_max_y <- y_max * 1.05
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(x = .data[[x]], y = .data[[y]], group = interaction(.data[[x]]))
  ) +
  scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
  scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
  labs(x = "x label", y = "y label")
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
p_data_extracted <- ggplot_build(p)$data[[1]]
ep_data_extracted <- ggplot_build(ep)$data[[1]]
expect_equal(p_data_extracted, ep_data_extracted)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# plot with fill
fillTheme <- "BuGn"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Accent"
legendTitleColour <- ""
app$set_inputs(`VIS-fill` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
aesColour <- aes()
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour, !!!aesFill,
      group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
    )
  ) +
  scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
  scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
  labs(x = "x label", y = "y label") +
  guides(fill = guide_legend(title = legendTitleFill)) +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_fill_brewer(palette = fillTheme) +
  scale_color_brewer(palette = colourTheme)
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# Adding colour to plot
app$set_inputs(`VIS-col` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
fillTheme <- "BuGn"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Accent"
colVar <- "Treatment"
legendTitleColour <- ""
app$set_inputs(`VIS-fill` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
aesColour <- aes(colour = .data[[colVar]])
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour, !!!aesFill,
      group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
    )
  ) +
  scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
  scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
  labs(x = "x label", y = "y label") +
  guides(fill = guide_legend(title = legendTitleFill)) +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_fill_brewer(palette = fillTheme) +
  scale_color_brewer(palette = colourTheme)
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)


# Set scales for y
padded_min_y <- 7.315
padded_max_y <- 26.3
app$set_inputs(`VIS-YRange` = c(7.315, 26.3))
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
fillTheme <- "BuGn"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Accent"
colVar <- "Treatment"
legendTitleColour <- ""
app$set_inputs(`VIS-fill` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
aesColour <- aes(colour = .data[[colVar]])
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour, !!!aesFill,
      group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
    )
  ) +
  scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
  scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
  labs(x = "x label", y = "y label") +
  guides(fill = guide_legend(title = legendTitleFill)) +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_fill_brewer(palette = fillTheme) +
  scale_color_brewer(palette = colourTheme)
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)


# Set additional the x axis
padded_min_x <- 47.5
padded_max_x <- 637
app$set_inputs(`VIS-XRange` = c(47.5, 637))
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
fillTheme <- "BuGn"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Accent"
colVar <- "Treatment"
legendTitleColour <- ""
app$set_inputs(`VIS-fill` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
aesColour <- aes(colour = .data[[colVar]])
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour, !!!aesFill,
      group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
    )
  ) +
  scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
  scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
  labs(x = "x label", y = "y label") +
  guides(fill = guide_legend(title = legendTitleFill)) +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_fill_brewer(palette = fillTheme) +
  scale_color_brewer(palette = colourTheme)
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# Test facet
app$set_inputs(`VIS-XRange` = c(47.5, 1250))
app$wait_for_idle()
app$set_inputs(`VIS-YRange` = c(7.315, 47.775))
app$wait_for_idle()
app$set_inputs(`VIS-facetBy` = "Type")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
fillTheme <- "BuGn"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Accent"
colVar <- "Treatment"
legendTitleColour <- ""
app$set_inputs(`VIS-fill` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
aesColour <- aes(colour = .data[[colVar]])
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
facetVar <- "Type"
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour, !!!aesFill,
      group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
    )
  ) +
  labs(x = "x label", y = "y label") +
  guides(fill = guide_legend(title = legendTitleFill)) +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_fill_brewer(palette = fillTheme) +
  scale_color_brewer(palette = colourTheme) +
  facet_wrap(~ .data[[facetVar]], scales = "free")
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)


# Change themes
app$set_inputs(`VIS-themeFill` = "Greys")
app$wait_for_idle()
app$set_inputs(`VIS-theme` = "Dark2")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
fillTheme <- "Greys"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Dark2"
colVar <- "Treatment"
legendTitleColour <- ""
app$set_inputs(`VIS-fill` = "Treatment")
app$wait_for_idle()
app$click("VIS-CreatePlotBox")
app$wait_for_idle()
aesColour <- aes(colour = .data[[colVar]])
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
facetVar <- "Type"
ep <- ggplot() +
  geom_boxplot(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour, !!!aesFill,
      group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
    )
  ) +
  labs(x = "x label", y = "y label") +
  guides(fill = guide_legend(title = legendTitleFill)) +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_fill_brewer(palette = fillTheme) +
  scale_color_brewer(palette = colourTheme) +
  facet_wrap(~ .data[[facetVar]], scales = "free")
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# Test scatterplots
app$set_inputs(VisConditionedPanels = "Scatterplot")
app$wait_for_idle()
app$click("VIS-CreatePlotScatter")
app$wait_for_idle()
colourTheme <- "Dark2"
colVar <- "Treatment"
legendTitleColour <- "Title colour"
aesColour <- aes(colour = .data[[colVar]])
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
facetVar <- "Type"
app$wait_for_idle()
ep <- ggplot() +
  geom_point(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour
    )
  ) +
  labs(x = "x label", y = "y label") +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_color_brewer(palette = colourTheme) +
  facet_wrap(~ .data[[facetVar]], scales = "free")
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)


# Test line plots
app$set_inputs(VisConditionedPanels = "Lineplot")
app$wait_for_idle()
app$click("VIS-CreatePlotLine")
app$wait_for_idle()
colourTheme <- "Dark2"
colVar <- "Treatment"
legendTitleColour <- "Title colour"
aesColour <- aes(colour = .data[[colVar]])
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
facetVar <- "Type"
ep <- ggplot() +
  geom_line(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour,
      group = interaction(.data[[x]], !!!aesColour)
    )
  ) +
  labs(x = "x label", y = "y label") +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_color_brewer(palette = colourTheme) +
  facet_wrap(~ .data[[facetVar]], scales = "free")
p <- app$get_values()$export$`VIS-plot`
app$wait_for_idle()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)

app$stop()
