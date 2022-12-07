
#' Import data
#'
#' @param df a data.frame returned from MTT::import
#' @return The LD50 and the quality of the fit for each substance
#' @examples
#' df <- import("./data/testfile1.xml")
#' ld50(df)
ld50 <- function(df) {

  neg <- df[df$names == "neg", ]
  pos <- df[df$names == "pos", ]
  neg <- as.numeric(as.character(neg$abs))
  pos <- as.numeric(as.character(pos$abs))
  pos <- mean(pos)
  neg <- mean(neg)

  df$abs <- as.numeric(as.character(df$abs))
  df$abs <- df$abs - pos
  df$abs <- df$abs*(1.0 / neg)

  ld50. <- function(df) {

    df$abs <- as.numeric(as.character(df$abs))
    df$conc <- as.numeric(as.character(df$conc))

    fit_log <- glm(abs ~ log(conc), data = df)
    fit_log_res <- broom::tidy(fit_log)
    # r squared
    rs_log <- 1.0 - (fit_log$deviance / fit_log$null.deviance)
    ld50_log <- MASS::dose.p(fit_log, p = 0.5)
    return(data.frame(ld50 = ld50_log[[1]], r2 = rs_log, SE = attributes(ld50_log)$SE[,1]) )
  }

  com_names <- unique(df$names)
  com_names <- com_names[com_names != "neg"]
  com_names <- com_names[com_names != "pos"]

  ld50s <- vapply(com_names,
                  FUN.VALUE = data.frame(ld50 = numeric(1), r2 = numeric(1), SE = numeric(1)),
    function(x) {
    temp <- df[df$names == x, ]
    ret <- ld50.(temp)
    return(ret)
  })

  return(ld50s)
}




#' Import data
#'
#' @param df a data.frame returned from MTT::import
#' @return The LD50 and the quality of the fit for each substance
#' @examples
#' df <- import("./data/testfile1.xml")
#' ld50_fit(df)
ld50_fit <- function(df) {

  neg <- df[df$names == "neg", ]
  pos <- df[df$names == "pos", ]
  neg <- as.numeric(as.character(neg$abs))
  pos <- as.numeric(as.character(pos$abs))
  pos <- mean(pos)
  neg <- mean(neg)

  df$abs <- as.numeric(as.character(df$abs))
  df$abs <- df$abs - pos
  df$abs <- df$abs*(1.0 / neg)

  ld50. <- function(df, nam) {

    df$abs <- as.numeric(as.character(df$abs))
    df$conc <- as.numeric(as.character(df$conc))

    fit_log <- glm(abs ~ log(conc), data = df)
    fit_log_res <- broom::tidy(fit_log)
    attr(fit_log_res, "name") <- nam
    return(fit_log_res)
  }

  com_names <- unique(df$names)
  com_names <- com_names[com_names != "neg"]
  com_names <- com_names[com_names != "pos"]

  ld50s <- lapply(com_names, function(x) {
    temp <- df[df$names == x, ]
    ret <- ld50.(temp, x)
    return(ret)
  })

  return(ld50s)
}
