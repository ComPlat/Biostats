#' Statistics of raw data
#'
#' @param df a data.frame returned from MTT::import
#' @return The aov results of the raw data
#' @examples
#' df <- import("./data/testfile1.xml")
#' stats(df)
stats <- function(df) {

  neg <- df[df$names == "neg", ]
  pos <- df[df$names == "pos", ]
  neg <- as.numeric(as.character(neg$abs))
  pos <- as.numeric(as.character(pos$abs))
  pos <- mean(pos)
  neg <- mean(neg)

  df$abs <- as.numeric(as.character(df$abs))
  df$abs <- df$abs - pos
  df$abs <- df$abs*(1.0 / neg)

  stats. <- function(df) {
    n <- unique(df$names)
    if(length(n) == 1L) {
      return()
    }

    df$abs <- as.numeric(as.character(df$abs))
    df$conc <- as.numeric(as.character(df$conc))

    p <- broom::tidy(aov(abs ~ conc, data = df))
    p <- list(n, p)
    return(p)
  }

  stat_res <- list()
  for(i in unique(df$names)) {
    if( i == "pos" ) {
      next
    }

    temp <- df[df$names == i, ]
    neg <- df[df$names == "neg", ]
    neg$conc <- 0

    temp <- rbind(temp, neg)
    stat_res[[length(stat_res) + 1]] <- stats.(temp)
  }

  return(stat_res)
}

