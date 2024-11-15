library(ggplot2)
library(shinytest2)
library(tinytest)
app <- bs::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "bs")
)
app$set_inputs(conditionedPanels = "Visualisation")
app$set_inputs(`VIS-yVar` = "uptake")
app$set_inputs(`VIS-xVar` = "conc")
app$click("VIS-CreatePlotBox")

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
ep <- ggplot(
) +
  geom_boxplot(
    data = CO2,
    aes(x = .data[[x]], y = .data[[y]], group = interaction(.data[[x]]))
  ) +
  scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
  scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
  labs(x = "x label", y = "y label")
p <- app$get_values()$export$`VIS-plot`
p_data_extracted <- ggplot_build(p)$data[[1]]
ep_data_extracted <- ggplot_build(ep)$data[[1]]
identical(p_data_extracted, ep_data_extracted)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
identical(pl$mapping, epl$mapping)

app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-fill` = "uptake")
app$set_inputs(`VIS-fill` = "Treatment")
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-col` = "Type")
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-YRange` = c(7.315, 26.3))
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-XRange` = c(47.5, 637))
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-XRange` = c(47.5, 1250))
app$set_inputs(`VIS-YRange` = c(7.315, 47.775))
app$set_inputs(`VIS-facetBy` = "Type")
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-facetScales` = "fixed")
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-themeFill` = "Greys")
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-theme` = "Dark2")
app$click("VIS-CreatePlotBox")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(VisConditionedPanels = "Scatterplot")
app$click("VIS-CreatePlotScatter")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-facetBy` = "")
app$click("VIS-CreatePlotScatter")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-XRange` = c(47.5, 598))
app$click("VIS-CreatePlotScatter")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-YRange` = c(7.315, 24.8))
app$click("VIS-CreatePlotScatter")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(VisConditionedPanels = "Lineplot")
app$set_inputs(`VIS-XRange` = c(47.5, 1250))
app$set_inputs(`VIS-YRange` = c(7.315, 47.775))
app$click("VIS-CreatePlotLine")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-facetBy` = "Type")
app$click("VIS-CreatePlotLine")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-facetBy` = "")
app$click("VIS-CreatePlotLine")
app$set_inputs(`VIS-XRange` = c(47.5, 648))
app$click("VIS-CreatePlotLine")
app$expect_values(output = "VIS-plotResult")
app$set_inputs(`VIS-YRange` = c(7.315, 19.3))
app$click("VIS-CreatePlotLine")
app$expect_values(output = "VIS-plotResult")
app$click("VIS-plotSave")
app$set_inputs(`VIS-TableSaved` = "Plot Nr 1 Type:  line")
app$set_inputs(`VIS-user_filename` = "Plot.xlsx")
app$click("VIS-downloadViss")
app$set_inputs(`VIS-user_filename` = "Plot.zip")
app$click("VIS-downloadViss")
