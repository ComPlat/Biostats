df <- read.csv("bs/inst/test_data/lc50_testfile.csv", header = TRUE, sep = ";")
head(df)
res <- bs:::ic50(df, "abs", "conc", "names", "neg", "pos")
resDF <- lapply(res, function(x) {
  if (inherits(x, "errorClass")) {
    return(NULL)
  }
  return(x[[1]])
})
resDF <- resDF[!is.null(resDF)]
resDF <- resDF[!sapply(resDF, is.null)]
resDF <- Reduce(rbind, resDF)
resP <- lapply(res, function(x) {
  if (inherits(x, "errorClass")) {
    return(NULL)
  }
  return(x[[2]])
})
resP <- resP[!is.null(resP)]
resP <- resP[!sapply(resP, is.null)]

bs:::create_plot_pages(resP)
