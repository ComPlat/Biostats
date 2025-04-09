HistorySidebarUI <- function(id) {
  ui <-tabPanel(
    "History",
    div(
      actionButton(NS(id, "replay_history"), "Replay history", class = "add-button",
        title = "Copy the history (json format) into the text field and apply it to the current data set"),
      class = "boxed-output"
    )
  )
}

HistoryEditorUI <- function(id) {
  ui <- fluidPage(
    div(
      textAreaInput(NS(id, "history_string"), "Operation:", value = "", rows = 12),
      class = "boxed-output"
    )
  )
}

HistoryEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$replay_history, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      result <- eval_history(input$history_string, DataModelState$df)
      if (is.null(result)) {
        return()
      }

      DataModelState$df <- result$DataModelState$df
      DataModelState$formula <- result$DataModelState$formula
      DataModelState$backup_df <- result$DataModelState$backup_df
      DataModelState$filter_col <- result$DataModelState$filter_col
      DataModelState$filter_group <- result$DataModelState$filter_group

      ResultsState$curr_data <- result$ResultsState$curr_data
      ResultsState$curr_name <- result$ResultsState$curr_name
      ResultsState$all_data <- result$ResultsState$all_data
      ResultsState$all_names <- result$ResultsState$all_names
      ResultsState$history <- result$ResultsState$history

      DataWranglingState$df <- result$DataWranglingState$df
      DataWranglingState$df_name <- result$DataWranglingState$df_name
      DataWranglingState$current_page <- result$DataWranglingState$current_page
      DataWranglingState$total_pages <- result$DataWranglingState$total_pages
      DataWranglingState$counter_id <- result$DataWranglingState$counter_id
      DataWranglingState$intermediate_vars <- result$DataWranglingState$intermediate_vars
    })

  })
}

