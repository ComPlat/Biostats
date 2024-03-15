visSidebarUI <- function(id) {
  tabPanel(
    "Visualisatin",
    textInput(NS(id , "yVar"), "Y variable", value = "y"),
    textInput(NS(id, "xVar"), "X variable", value = "x"),
    radioButtons(NS(id, "xType"), "Type of x",
                 choices = c(
                   factor = "factor",
                   numeric = "numeric"
                 ),
                 selected = "factor"
    ),
    textInput(NS(id, "xaxisText"), "X axis label", value = "x label"),
    textInput(NS(id, "yaxisText"), "Y axis label", value = "y label"),
    selectInput(NS(id, "fit"), "Choose a fitting method",
                c(
                  "none" = "none",
                  "lm" = "lm",
                  "glm" = "glm",
                  "gam" = "gam",
                  "loess" = "loess"
                ),
                selectize = FALSE
    ),
    textInput(NS(id, "fill"), "Fill variable"),
    textInput(NS(id, "legendtitleFill"), "Legend title for fill", value = "Title fill"),
    textInput(NS(id, "col"), "Colour variable"),
    textInput(NS(id, "legendtitleCol"), "Legend title for colour", value = "Title colour"),
    selectInput(NS(id, "theme"), "Choose a 'colour' theme",
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
    selectInput(NS(id, "themeFill"), "Choose a 'fill' theme",
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
    radioButtons(NS(id, "facet_modeLine"),
                 "Choose Facet Mode:",
                 choices = c("none", "facet_wrap", "facet_grid")
    ),
    textInput(NS(id, "facetLine"), "split plot by")
    )
}

visUI <- function(id) {
  fluidRow(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "download.js")
    ),
    tabsetPanel(
      tabPanel("Boxplot",
          actionButton("CreateBoxplot", "Create plot")
      ),
      tabPanel("Scatterplot",
          actionButton("CreateSactterplot", "Create plot")
      ),
      tabPanel("Lineplot",
          actionButton("CreateLineplot", "Create plot")
      ),
      id = "VisConditionedPanels"   
    )
  )
}

visServer <- function(id, df, listResults) {
  moduleServer(id, function(input, output, session) {  
  })
}