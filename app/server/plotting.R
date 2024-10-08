plotting <- function(id, var, result, res, plot_res) {
  moduleServer(
    id,
    function(input, output, session) {
      plot_res <- reactiveValues(data = NULL)

      plotfct <- function(method) {
        df <- var$df
        req(df)
        x <- NULL
        y <- NULL
        col <- ""
        fill <- ""
        fill_title <- ""
        col_title <- ""
        xlabel <- ""
        ylabel <- ""
        xtype <- NULL
        theme <- NULL
        theme_fill <- NULL
        facet_mode <- NULL
        facet <- NULL
        fitMethod <- NULL
        width <- input$widthPlot
        height <- input$heightPlot
        resolution <- input$resPlot
        if (width <= 0) {
          showNotification(paste("width has to be a positive number is changed to 10 cm"), duration = 0)
          width <- 10
        }
        if (height <= 0) {
          showNotification(paste("height has to be a positive number is changed to 10 cm"), duration = 0)
          height <- 10
        }
        if (width > 100) {
          showNotification(paste("width exceeds max value of 100 cm. Is set to 100 cm."), duration = 0)
          width <- 100
        }
        if (height > 100) {
          showNotification(paste("height exceeds max value of 100 cm. Is set to 100 cm."), duration = 0)
          height <- 100
        }

        if (method == "box") {
          x <- input$xBox
          y <- input$yBox
          col <- input$colBox
          fill <- input$fillBox
          fill_title <- input$legendtitle_colBox
          col_title <- input$legendtitle_fillBox
          xlabel <- input$xaxis_textBox
          ylabel <- input$yaxis_textBox
          xtype <- input$xtypeBox
          theme <- input$themeBox
          theme_fill <- input$theme_fillBox
          facet_mode <- input$facet_modeBox
          facet <- input$facetBox
        } else if (method == "dot") {
          x <- input$xDot
          y <- input$yDot
          col <- input$colDot
          col_title <- input$legendtitle_colDot
          xlabel <- input$xaxis_textDot
          ylabel <- input$yaxis_textDot
          xtype <- input$xtypeDot
          theme <- input$themeDot
          facet_mode <- input$facet_modeDot
          facet <- input$facetDot
          fitMethod <- input$fitDot
        } else if (method == "line") {
          x <- input$xLine
          y <- input$yLine
          col <- input$colLine
          col_title <- input$legendtitle_colLine
          xlabel <- input$xaxis_textLine
          ylabel <- input$yaxis_textLine
          xtype <- input$xtypeLine
          theme <- input$themeLine
          facet_mode <- input$facet_modeLine
          facet <- input$facetLine
        }
        if (is.null(x)) showNotification("please specify the X variable", duration = 0)
        if (is.null(y)) showNotification("please specify the Y variable", duration = 0)
        req(!is.null(x))
        req(!is.null(y))
        col_names <- names(df)
        check_x <- x %in% col_names
        check_y <- y %in% col_names
        if (!check_x) showNotification("X variable not found", duration = 0)
        if (!check_y) showNotification("Y variable not found", duration = 0)
        req(check_x)
        req(check_y)
        if (!(fill %in% names(df)) && (fill != "")) showNotification("fill variable not found", duration = 0)
        if (!(col %in% names(df)) && (fill != "")) showNotification("colour variable not found", duration = 0)
        req((fill %in% names(df)) || (fill == ""))
        req((col %in% names(df)) || (col == ""))

        xd <- NULL
        if (xtype == "numeric") {
          xd <- as.numeric(df[, x])
        } else {
          xd <- as.factor(df[, x])
        }
        yd <- as.numeric(df[, y])

        if (fitMethod != "none" && !is.null(fitMethod) && xtype != "numeric") {
          # fitMethod <- "none"
          showNotification("Fit method will be ignored as X variable is not numerical", duration = 0)
        }
        pfct <- function() {
          if ((fill == "") && (col == "")) {
            p <- ggplot(data = df, aes(x = xd, y = yd)) +
              ylab(ylabel) +
              xlab(xlabel)
          } else if ((fill != "") && (col != "")) {
            p <- ggplot(
              data = df,
              aes(
                x = xd, y = yd,
                fill = df[, fill], color = df[, col]
              )
            ) +
              ylab(ylabel) +
              xlab(xlabel) +
              guides(fill = guide_legend(title = fill_title), col = guide_legend(title = col_title))
          } else if ((fill != "") && (col == "")) {
            p <- ggplot(
              data = df,
              aes(
                x = xd, y = yd,
                fill = df[, fill]
              )
            ) +
              ylab(ylabel) +
              xlab(xlabel) +
              guides(fill = guide_legend(title = fill_title))
          } else if ((fill == "") && (col != "")) {
            p <- ggplot(
              data = df,
              aes(
                x = xd, y = yd,
                color = df[, col]
              )
            ) +
              ylab(ylabel) +
              xlab(xlabel) +
              guides(col = guide_legend(title = col_title))
          }
          if (method == "box") {
            p <- p +
              scale_color_brewer(palette = theme) +
              scale_fill_brewer(palette = theme_fill)
            p <- p + geom_boxplot()
          } else if (method == "dot") {
            p <- p +
              scale_color_brewer(palette = theme)
            p <- p + geom_point() + geom_smooth(method = fitMethod)
            if (fitMethod != "" && !is.null(fitMethod) && fitMethod != "none") p <- p + stat_poly_eq(ggpmisc::use_label(c("eq", "n", "R2", "p", "F")))
          } else if (method == "line") {
            p <- p +
              scale_color_brewer(palette = theme)
            p <- p + geom_line()
          }
          if (facet_mode == "facet_wrap") {
            p <- p + facet_wrap(~ df[, facet], scales = "free")
          } else if (facet_mode == "facet_grid") {
            p <- p + facet_grid(. ~ df[, facet], scales = "free")
          }

          return(p)
        }
        p <- tryCatch(
          {
            p <- pfct()
          },
          warning = function(warn) {
            showNotification(paste("A warning occurred: ", conditionMessage(warn)), duration = 0)
          },
          error = function(err) {
            showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
          }
        )
        result$d <- plotResult$new(p, width, height, resolution)
        plot_res$data <- p
        result$curr_name <- paste("Plot Nr", length(result$names) + 1, paste("Type: ", method))
        output$plot_res <- renderPlot(p)
      }

      observeEvent(input$boxplot, {
        plotfct("box")
      })

      observeEvent(input$dotplot, {
        plotfct("dot")
      })

      observeEvent(input$lineplot, {
        plotfct("line")
      })

      output$plot_result <- renderPlot({
        plot_res$data
      })
    }
  )
}
