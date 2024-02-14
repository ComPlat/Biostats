

library(ggplot2)
library(ggdendro)
library(tibble)
library(patchwork)

model <- hclust(dist(USArrests), "ave")
df <- USArrests 
df$label <- row.names(df)
df <- tidyr::pivot_longer(df, cols = -label) %>% as.data.frame()
label_orders <- data.frame(label = model$label, order = model$order)
df$order <- ordered <- label_orders[match(df$label, label_orders[, 1]), 2]
df$ordered_labels <- ordered <- label_orders[match(df$label, label_orders[, 1]), 1]

dhc <- as.dendrogram(model)
ddata <- dendro_data(dhc, type = "rectangle")

p1 <- ggplot(df) + 
  geom_tile(aes(name, order, fill = value)) +
  scale_y_continuous(breaks = df$order, labels = df$ordered_labels) 

p2 <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(aes(x = x, y = y, label = label, hjust = 0), data = label(ddata)) +
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))

p1 | p2

f <- tempfile(fileext = ".png")
png(file = f) 
p <- capture.output(pheatmap::pheatmap(USArrests, scale = "column", cluster_rows = FALSE, cluster_cols = FALSE))
dev.off()
p <- png::readPNG(f)
plot(1:2, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
rasterImage(p, 0, 0, 1, 1)
unlink(f)

pheatmap::pheatmap(CO2, scale = "column", cluster_rows = FALSE, cluster_cols = FALSE)


ggplot(data = mtcars, aes(x = cyl, y = mpg)) +
    geom_point() +
    geom_smooth()