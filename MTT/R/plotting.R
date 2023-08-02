#' LD50 plots
#'
#' @param df a data.frame returned from MTT::import
#' @return The LD50 plots
#' @examples
#' df <- import("./data/testfile1.xml")
#' ld50plot(df)
ld50plot <- function(df) {

  neg <- df[df$names == "neg", ]
  pos <- df[df$names == "pos", ]
  neg <- as.numeric(as.character(neg$abs))
  pos <- as.numeric(as.character(pos$abs))
  pos <- mean(pos)
  neg <- mean(neg)

  df$abs <- as.numeric(as.character(df$abs))
  df$abs <- df$abs - pos
  df$abs <- df$abs*(1.0 / neg)

  ploter <- function(df) {

    df$abs <- as.numeric(as.character(df$abs))
    df$conc <- as.numeric(as.character(df$conc))

    fit_lin <- glm(abs ~ conc, data = df)
    fit_log <- glm(abs ~ log(conc), data = df)

    # r squared
    rs_log <- 1.0 - (fit_log$deviance / fit_log$null.deviance)
    rs_lin <- 1.0 - (fit_lin$deviance / fit_lin$null.deviance)

    ld50_log <- MASS::dose.p(fit_log, p = 0.5)
    ld50_lin <- MASS::dose.p(fit_lin, p = 0.5)

    pred <- NULL
    ld50 <- NULL
    max_conc <- max(df$conc)
    if(rs_log > rs_lin) {
      pred <- predict(fit_lin, data.frame(conc = seq(0.1, max_conc, 0.2),
                                      abs = NA) )
      ld50 <- ld50_lin
    } else {
      pred <- predict(fit_log, data.frame(conc = seq(0.1, max_conc, 0.2),
                                      abs = NA) )
      ld50 <- ld50_log
    }
    pred <- data.frame(conc = seq(0.1, 50, 0.2), abs = pred)

    require(ggplot2)
    p <- ggplot(data = df,
                aes(x = conc, y = abs)) +
      #geom_point(aes(x = ld50[1], y = 0.5), colour = "cyan") +
      geom_line(data = pred, aes(x = conc, y = abs, color = "red")) +
      geom_point() +
      theme(legend.position = "none") +
      xlab(paste(unique(df$names), "[µM]") )
    return(p)
  }

  trash <- lapply(unique(df$names), function(x) {
    if( (x == "neg")  ||
        (x == "pos") ) {
      return()
    }

    temp <- df[df$names == x, ]
    return(ploter(temp))
  })
  trash[sapply(trash, is.null)] <- NULL
  return(trash)
}


#' Boxplots
#'
#' @param df a data.frame returned from MTT::import
#' @return The boxplots of the raw data
#' @examples
#' df <- import("./data/testfile1.xml")
#' box_plots(df)
box_plots <- function(df) {

  neg <- df[df$names == "neg", ]
  pos <- df[df$names == "pos", ]
  neg <- as.numeric(as.character(neg$abs))
  pos <- as.numeric(as.character(pos$abs))
  pos <- mean(pos, na.rm = TRUE)
  neg <- mean(neg, na.rm = TRUE)

  df$abs <- as.numeric(as.character(df$abs))
  df$abs <- df$abs - pos
  df$abs <- df$abs*(1.0 / neg)

  ploter <- function(df) {
    n <- unique(df$names)
    df$abs <- as.numeric(as.character(df$abs))
    df$conc <- as.numeric(as.character(df$conc))

    fit_log <- glm(abs ~ log(conc), data = df)

    max_conc <- max(df$conc)

    pred <- predict(fit_log, data.frame(conc = seq(0.1, max_conc, 0.2),
                                        abs = NA) )
    pred <- data.frame(conc = seq(0.1, max_conc, 0.2), abs = pred)

    require(ggplot2)
    p1 <- ggplot() +
      geom_boxplot(data = df,
                   aes(x = as.factor(conc), y = abs*100, group = conc)) +
      xlab(paste(unique(df$names), "[µM]") ) +
      ylab("viability [%]") +
      ggtitle(df$names) +
      theme(text = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 14, face = "bold") )
    p2 <- ggplot() +
      geom_line(data = pred,
                  aes(x = conc, y = abs*100)) +
      stat_summary(fun = "median", data = df,
                   colour = "darkred", size = .5,
                   position = position_dodge(.9),
                   aes(group = conc,  x = conc, y = abs*100) ) +
      xlab(paste(unique(df$names), "[µM]") ) +
      ylab("viability [%]") +
      ggtitle(df$names) +
      theme(text = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 14, face = "bold") )
    p <- cowplot::plot_grid(p1, p2, ncol = 1)
    return(p)
  }

  plots <- list()
  for(i in unique(df$names)) {
    if( i == "pos" || i == "neg") {
      next
    }

    temp <- df[df$names == i, ]
    plots[[length(plots) + 1]] <- ploter(temp)
  }

  return(plots)
}

