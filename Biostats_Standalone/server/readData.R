upload <- function(path) {
  req(is.character(path))
  df <- NULL
    df <- try( as.data.frame(read_excel(path, col_names = TRUE)), silent = TRUE)
  if (class(df) == "try-error") {
    # identify seperator
    line <- readLines(path, n = 1)
    semicolon <- grepl(";", line)
    comma <- grepl(",", line)
    tab <- grepl("\t", line)
    seperator <- NULL
    if (semicolon == TRUE) {
      seperator <- ";"
    } else if (comma == TRUE) {
      seperator <- ","
    } else if (tab == TRUE) {
      seperator <- "\t"
    } else {
      return("error")
    }
    df <- try(read.csv(path, header = TRUE, sep = seperator))
    if (class(df) == "try-error") {
      return("error")
    }
  } else {
    f <- function(x) {
      options(warn = -1)
      x <- as.numeric(x)
      options(warn = 0)
      x <- x[!is.na(x)]
      length(x) > 0
    }
    check <- apply(df, 2, f)
    conv <- function(a, b) {
      if (a == TRUE) {
        return(as.numeric(b))
      }
      return(b)
    }
    df <- Map(conv, check, df)
    df <- data.frame(df)
  }
  return(df)
}
