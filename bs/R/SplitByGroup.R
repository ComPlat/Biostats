SplitByGroupUI <- function(id) {
  ui <- fluidPage(
    fluidRow(
      div(
        actionButton(
          NS(id, "split_docu"),
          label = NULL,
          icon = icon("question-circle")
        ),
        uiOutput(NS(id, "colnames_dropdown")),
        class = "boxed-output"
      ),
      div(
        uiOutput(NS(id, "levels_dropdown")),
        class = "boxed-output"
      ),
      actionButton(NS(id, "split_data"), "Split data")
    )
  )
}

SplitByGroupServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive values
    r_vals <- reactiveValues(
      df = NULL,
      is_filtered = FALSE
    )

    observe({
      r_vals$df <- data$df
    })

    # Create colnames dropdown
    output[["colnames_dropdown"]] <- renderUI({
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      colnames <- names(r_vals$df)
      tooltip <- "Select the column by name which you want to split by"
      div(
        tags$label(
          "Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("SG-colnames-dropdown_"),
          label = "Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL,
          multiple = TRUE
        )
      )
    })

    # Show levels based on column which is choosen
    output[["levels_dropdown"]] <- renderUI({
      req(!is.null(r_vals$df))
      req(is.data.frame(r_vals$df))
      selected_col <- input[[paste0("colnames-dropdown_")]]
      if (is.null(selected_col)) {
        return(NULL)
      }
      vals <- unique(r_vals$df[, selected_col])
      tooltip <- "Select the level (group) by name which you want to use"
      div(
        tags$label(
          "Variable levels",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("SG-levels-dropdown_"),
          label = "Variable levels",
          choices = vals[1:length(vals)],
          selected = NULL,
          multiple = TRUE
        )
      )
    })

    # React to split data
    observeEvent(input$split_data, {
      print_req(is.data.frame(r_vals$df), "The dataset is missing")
      e <- try({
        selected_cols <- input[[paste0("colnames-dropdown_")]]
        selected_groups <- input[[paste0("levels-dropdown_")]]
        if (length(selected_groups) == 0 || length(selected_cols) == 0) {
          stop("Invalid subset either no columns or now levels of the columns were selected")
        }
        data$backup_df <- r_vals$df
        data$df <- split(r_vals$df, selected_cols, selected_groups)
        data$filter_col <- selected_cols
        data$filter_group <- selected_groups
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        print_err(e)
      }
    })
  })
}
