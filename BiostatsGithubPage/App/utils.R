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
