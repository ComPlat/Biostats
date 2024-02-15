#' Creates report file
#'
#' @param boxplots result of box_plots()
#' @param ld50_values result of ld50()
#' @param ld50fit result of ld50 fit summary
#' @param text the variable text which has to be inserted in the document
#' @param filename name of excel file
#' @return Nothing is returned. Just called for its sideeffects
report_pdf <- function(boxplots, ld50_values, ld50fit, stat_aov, text, file_name) {
  library(quarto)
  file <- tempfile()
  saveRDS(list(boxplots, ld50_values, ld50fit, stat_aov, text),
          file =  file)
  path <- system.file("examples", package = "MTT")
  rmarkdown::render(paste0(path, "/template_pdf.Rmd"),
                   params = list(file = file), output_file = file_name)
}
