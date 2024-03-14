plotting <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "Visualisation",
    fluidRow(
      tabBox(
        tabPanel(
          "Boxplots",
          box(
            textInput(ns("yBox"), "Y variable", value = "y"),
            textInput(ns("xBox"), "X variable", value = "x"),
            radioButtons(ns("xtypeBox"), "Type of x",
              choices = c(
                factor = "factor",
                numeric = "numeric"
              ),
              selected = "factor"
            ),
            textInput(ns("xaxis_textBox"), "X axis label", value = "x label"),
            textInput(ns("yaxis_textBox"), "Y axis label", value = "y label"),
            textInput(ns("fillBox"), "Fill variable"),
            textInput(ns("legendtitle_fillBox"), "Legend title for fill", value = "Title fill"),
            textInput(ns("colBox"), "Colour variable"),
            textInput(ns("legendtitle_colBox"), "Legend title for colour", value = "Title colour"),
            actionButton(ns("boxplot"), "Create plot"),
            selectInput(ns("themeBox"), "Choose a 'colour' theme",
              c(
                "BuPu" = "BuPu",
                "RdYIBu" = "RdYIBu",
                "Paired" = "Paired",
                "PuOr" = "PuOr",
                "Spectral" = "Spectral",
                "Pastel1" = "Pastel1",
                "hue" = "hue",
                "grey" = "grey"
              ),
              selectize = FALSE
            ),
            selectInput(ns("theme_fillBox"), "Choose a 'fill' theme",
              c(
                "BuPu" = "BuPu",
                "RdYIBu" = "RdYIBu",
                "Paired" = "Paired",
                "PuOr" = "PuOr",
                "Spectral" = "Spectral",
                "Pastel1" = "Pastel1",
                "hue" = "hue",
                "grey" = "grey"
              ),
              selectize = FALSE
            ),
            radioButtons(ns("facet_modeBox"),
              "Choose Facet Mode:",
              choices = c("none", "facet_wrap", "facet_grid")
            ),
            textInput(ns("facetBox"), "split plot by"),
            width = 24
          )
        ),
        tabPanel(
          "Dotplots",
          box(
            textInput(ns("yDot"), "Y variable", value = "y"),
            textInput(ns("xDot"), "X variable", value = "x"),
            radioButtons(ns("xtypeDot"), "Type of x",
              choices = c(
                factor = "factor",
                numeric = "numeric"
              ),
              selected = "factor"
            ),
            textInput(ns("xaxis_textDot"), "X axis label", value = "x label"),
            textInput(ns("yaxis_textDot"), "Y axis label", value = "y label"),
            textInput(ns("colDot"), "Colour variable"),
            textInput(ns("legendtitle_colDot"), "Legend title for colour", value = "Title colour"),
            selectInput(ns("fitDot"), "Choose a fitting method",
              c(
                "none" = "none",
                "lm" = "lm",
                "glm" = "glm",
                "gam" = "gam",
                "loess" = "loess"
              ),
              selectize = FALSE
            ),
            actionButton(ns("dotplot"), "Create plot"),
            selectInput(ns("themeDot"), "Choose a colour theme",
              c(
                "BuPu" = "BuPu",
                "RdYIBu" = "RdYIBu",
                "Paired" = "Paired",
                "PuOr" = "PuOr",
                "Spectral" = "Spectral",
                "Pastel1" = "Pastel1",
                "hue" = "hue",
                "grey" = "grey"
              ),
              selectize = FALSE
            ),
            radioButtons(ns("facet_modeDot"),
              "Choose Facet Mode:",
              choices = c("none", "facet_wrap", "facet_grid")
            ),
            textInput(ns("facetDot"), "split plot by"),
            width = 24
          )
        ),
        tabPanel(
          "Lineplots",
          box(
            textInput(ns("yLine"), "Y variable", value = "y"),
            textInput(ns("xLine"), "X variable", value = "x"),
            radioButtons(ns("xtypeLine"), "Type of x",
              choices = c(
                factor = "factor",
                numeric = "numeric"
              ),
              selected = "factor"
            ),
            textInput(ns("xaxis_textLine"), "X axis label", value = "x label"),
            textInput(ns("yaxis_textLine"), "Y axis label", value = "y label"),
            textInput(ns("colLine"), "Colour variable"),
            textInput(ns("legendtitle_colLine"), "Legend title for colour", value = "Title colour"),
            selectInput(ns("fitLine"), "Choose a fitting method",
              c(
                "none" = "none",
                "lm" = "lm",
                "glm" = "glm",
                "gam" = "gam",
                "loess" = "loess"
              ),
              selectize = FALSE
            ),
            actionButton(ns("lineplot"), "Create plot"),
            selectInput(ns("themeLine"), "Choose a colour theme",
              c(
                "BuPu" = "BuPu",
                "RdYIBu" = "RdYIBu",
                "Paired" = "Paired",
                "PuOr" = "PuOr",
                "Spectral" = "Spectral",
                "Pastel1" = "Pastel1",
                "hue" = "hue",
                "grey" = "grey"
              ),
              selectize = FALSE
            ),
            radioButtons(ns("facet_modeLine"),
              "Choose Facet Mode:",
              choices = c("none", "facet_wrap", "facet_grid")
            ),
            textInput(ns("facetLine"), "split plot by"),
            width = 24
          )
        )
      ),
      tabBox(
        tabPanel(
          "Save",
          box(
            actionButton("plot_save", "Add output to result-file"),
            textInput("plot_file_name", "Filename", value = "new_file.xlsx"),
            downloadButton("download_plot", "Save results"),
            checkboxGroupInput("TableSaved2", "Saved results to file", NULL),
            plotOutput(ns("plot_result")),
            imageOutput(ns("image_result")),
            numericInput(ns("widthPlot"), "Width of plot [cm]", value = 10),
            numericInput(ns("heightPlot"), "Height of plot [cm]", value = 10),
            numericInput(ns("resPlot"), "Resolution of plot", value = 300),
            width = 24
          )
        )
      )
    )
  )
}
