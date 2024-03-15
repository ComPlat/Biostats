p <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()
f <- tempfile(fileext = ".png")
ggsave(f, p)    
png_content <- readBin(f, "raw", n = 10000)
#png_content <- rawToChar(png_content)
writeBin(png_content, "output.png")
