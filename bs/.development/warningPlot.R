library(ggplot2)

df <- data.frame(
  x = rep(c("A", "B"), each = 10),
  y = c(1:9, Inf, 11:19, NA)
)

p <- tryCatch(
  {
      ggplot(df, aes(x, y)) +
        geom_boxplot() +
        ggtitle("Boxplot with Non-finite Values")
  },
  warning = function(w) {
    message("Warning captured: Non-finite values removed from the data")
    NULL
  },
  error = function(e) {
    message("Error captured: ", e$message)
    NULL
  }
)

if (!is.null(p)) print(p)


test <- tryCatch(
  {
    print(p)
  },
  warning = function(w) {
    message("Warning captured: Non-finite values removed from the data")
    return(w)
  },
  error = function(e) {
    message("Error captured: ", e$message)
    NULL
  }
)

test


