DF2String <- function(df) {
	resNames <- names(df)
	resNames <- paste(resNames, collapse = "\t")
	resNames <- paste(resNames, "\n")
	res <- apply(df, 1, function(x) {
		x <- as.character(x)
		x <- paste(x, collapse = "\t")
		return(x)
	})
	res <- paste0(resNames, "\n", res, collapse = "")
	res <- paste0(res, "\n")
}

setClass("plot", 
  slots = c(
    p = "ANY", 
    width = "numeric",
    height = "numeric",
    resolution = "numeric"
  )
)

createJSString <- function(l) {
	jsString <- character(length(l))
	for (i in seq_along(l)) {
        if (inherits(l[[i]], "plot")) {
          p <- l[[i]]@p
          width <- l[[i]]@width
          height <- l[[i]]@height
          resolution <- l[[i]]@resolution
          fn <- tempfile(fileext = '.png')
          ggsave(plot = p, filename = fn, width = width, height = height, dpi = resolution) 
          jsString[i] <- paste0("data:image/png;base64,", base64enc::base64encode(fn))
          unlink(fn)
        } else if (inherits(l[[i]], "data.frame")) {
          jsString[i] <- DF2String(l[[i]])
        } else if (is.character(l[[i]])) {
          jsString[i] <- l[[i]]
        }
  }
  return(jsString)
}

stackDF <- function(df, keepCol) {
  as.data.frame(pivot_longer(df, cols = -keepCol,
               names_to = "name", values_to = "value"))
}

unstackDF <- function(df, name, value) {
  df <- pivot_wider(df, names_from = name, values_from = value)
  df <- map(df, simplify) %>% 
    as.data.frame()
  as.data.frame(df)
}

