#' Saves data
#'
#' @param ld50plots result of ld50plot()
#' @param boxplots result of box_plots()
#' @param ld50_values result of ld50()
#' @param ld50fit_res result of summary of fit
#' @param filename name of excel file
#' @return The aov results of the raw data
#' @examples
#' df <- import("./data/testfile1.xml")
#' stats(df)
save <- function(ld50plots, boxplots, ld50_values, ld50fit_res, file_name) {
  require(openxlsx)

  f <- function(pl, factor) {

    start_row <- 1

    for(i in seq_along(pl)) {
      if(is.null(pl[[i]])) {
        next
      }
      print(pl[i])

      insertPlot(wb, 1, "Plots", xy = c("A", start_row), width = 16,
                 height = 10, fileType = "png", units = "cm")

      start_row <- start_row + 25

    }
  }


  f2 <- function(ar) {
    start_row <- 1

    for(i in seq_along(ar)) {
      if(is.null(ar[[i]])) {
        next
      }
      writeData(wb, "ld50fit",
                attributes(ar[[i]])$name,
                startCol = "A",
                startRow = start_row, colNames = TRUE, rowNames = TRUE)
      writeData(wb, "ld50fit",
                ar[[i]],
                startCol = "A",
                startRow = start_row + 1, colNames = TRUE, rowNames = TRUE)
      start_row <- start_row + 10

    }
  }


  wb <- createWorkbook()
  addWorksheet(wb, "Plots")
  addWorksheet(wb, "LD50")
  addWorksheet(wb, "ld50fit")
  f(c(ld50plots, boxplots), 15)
  writeData(wb, "LD50", ld50_values, colNames = TRUE, rowNames = TRUE)
  f2(ld50fit_res)
  saveWorkbook(wb, file_name, overwrite = TRUE)

}






