library(RefManageR)
bib <- c(
  bibtype = "book",
  key = "Seber1989",
  title = "Nonlinear Regression",
  author = "Seber, G. A. F. and Wild, C. J",
  publisher = "Wiley & Sons",
  address = "New York",
  pages = "330",
  date = "1989"
)
bib_entry <- as.BibEntry(bib)
toBibtex(bib_entry)
