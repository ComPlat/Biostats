history_to_table <- function(history) {
  hist <- lapply(history, function(step) {
    result_name <- ifelse(is.null(step[["Result name"]]), "", step[["Result name"]])
    df <- data.frame(Step = step$type, Result = result_name)
    step <- step[!names(step) %in% c("type", "Result name")]
    names_step <- names(step)
    df$details <- paste(names_step, step, sep = ": ", collapse = "; ")
    df
  })
  hist <- do.call(rbind, hist) |> as.data.frame()
  list(hist) # NOTE: required to add it to result list otherwise the dataframe is split
}

eval_entry <- function(entry, DataModelState, DataWranglingState, ResultsState) {
  res <- NULL
  switch(
    entry$type,
    Correlation = {
      res <- correlation$new(
        DataModelState$df,
        as.formula(entry[["formula"]]),
        entry[["Correlation method"]],
        entry[["alternative hypothesis"]],
        entry[["Confidence level of the interval"]],
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    Visualisation = {
      res <- visualisation$new(
        df = DataModelState$df, x = entry$x, y = entry$y, method = entry[["Plot-type"]],
        xlabel = entry[["X axis label"]], type_of_x = entry[["Type of x"]], ylabel = entry[["Y axis label"]],
        colour_var = entry[["Colour variable"]], colour_legend_title = entry[["Legend title for colour"]], colour_theme = entry[["Colour theme"]],
        fill_var = entry[["Fill variable"]], fill_legend_title = entry[["Legend title for fill"]], fill_theme = entry[["Fill theme"]],
        facet_var = entry[["Split by"]], facet_y_scaling = entry[["How to scale y in subplots"]],
        xrange = char_to_orig_type(entry[["X-Range"]]), yrange = char_to_orig_type(entry[["Y-Range"]]),
        width = as.numeric(entry["Width"]), height = as.numeric(entry[["Height"]]), resolution = as.numeric(entry[["Resolution"]]),
        com = backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    ApplyFilter = {
      res <- apply_filter$new(
        entry[["variable"]], entry[["variable levels"]],
        backend_communicator
      )
      res$validate()
      res$eval(DataModelState, ResultsState)
    },
    RemoveFilter = {
      res <- remove_filter$new()
      res$validate()
      res$eval(DataModelState, ResultsState)
    },
    CreateIntermediateVariable = {
      res <- create_intermediate_var$new(
        DataWranglingState$df, DataWranglingState$df_name,
        DataWranglingState$intermediate_vars,
        entry[["operation"]], entry[["name"]],
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState)
    },
    RemoveIntermediateVariable = {
      res <- remove_intermediate_var$new(
        entry[["Intermediate variable"]],
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState)
    },
    CreateNewColumn = {
      res <- create_new_col$new(
        DataWranglingState$df, DataWranglingState$df_name,
        DataWranglingState$intermediate_vars,
        entry[["operation"]], entry[["column name"]],
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState, DataModelState)
    },
    CreateFormula = {
      f <- entry[["formula"]] |> as.formula()
      res <- create_formula$new(
        f[2], f[3], DataModelState$df,
        backend_communicator
      )
      res$validate()
      res$eval(DataModelState)
    },
    ModelSummary = {
      res <- summarise_model$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        backend_communicator)
      res$validate()
      res$eval(ResultsState)
    },
    ShapiroOnData = {
      res <- shapiro_on_data$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    ShapiroOnResiduals = {
      res <- shapiro_on_residuals$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    LeveneTest = {
      res <- levene$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        entry[["Data center"]],
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    DiagnosticPlots = {
      res <- diagnostic_plots$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    DoseResponse = {
      res <- dose_response$new(
        DataModelState$df,
        entry[["outliers"]],
        entry[["Log transform x-axis"]],
        entry[["Log transform y-axis"]],
        entry[["Column containing the names"]],
        as.formula(entry[["formula"]]),
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    TTest = {
      res <- t_test$new(
        DataModelState$df,
        as.formula(entry[["formula"]]),
        entry[["The two variances are"]],
        entry[["Confidence level of the interval"]],
        entry[["alternative hypothesis"]],
        backend_communicator
      )
      res$validate()
      res$eval(ResultsState)
    },
    ANOVA = {
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        NULL, NULL, NULL, backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "aov")
    },
    `Kruskal-Wallis Test` = {
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        NULL, NULL, NULL, backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "kruskal")
    },
    `Tukey HSD` = {
      bal <- "Unbalanced"
      if (entry[["Balanced design"]]) bal <- "Balanced"
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        bal, entry[["P-value"]], NULL, backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "HSD")
    },
    `Kruskal Wallis post hoc test` = {
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        NULL, entry[["P-value"]], entry[["Adjusted p value method"]], backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "kruskalTest")
    },
    `Least significant difference test` = {
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        NULL, entry[["P-value"]], entry[["Adjusted p value method"]], backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "LSD")
    },
    `Scheffe post hoc test` = {
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        NULL, entry[["P-value"]], NULL, backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "scheffe")
    },
    `REGW post hoc test` = {
      res <- statistical_tests$new(
        DataModelState$df, as.formula(entry[["formula"]]),
        NULL, entry[["P-value"]], NULL, backend_communicator
      )
      res$validate()
      res$eval(ResultsState, "REGW")
    },
    RemoveResult = {
      res <- remove_result$new(entry[["Entry removed"]])
      res$validate()
      res$eval(ResultsState)
    }
  )
  if (is.null(res)) {
    stop(sprintf("Unknown entry in history called: %s", entry$type))
  }
  return(res)
}

eval_history <- function(json_string, df) {
  e <- try({
    l <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
    result_state <- backend_result_state$new()
    data_model_state <- backend_data_model_state$new(df)
    data_wrangling_state <- backend_data_wrangling_state$new(data_model_state)
    for (i in seq_along(l)) {
      inner_e <- try({
        eval_entry(l[[i]], data_model_state, data_wrangling_state, result_state)
      })
      if (inherits(inner_e, "try-error")) {
        err <- conditionMessage(attr(inner_e, "condition"))
        print_err(sprintf("Error in step: %s", i))
        stop(err)
      }
    }
  }, silent = TRUE)
  if (inherits(e, "try-error")) {
    err <- conditionMessage(attr(e, "condition"))
    print_err(err)
    return()
  }

  return(
    list(
      ResultsState = result_state,
      DataModelState = data_model_state,
      DataWranglingState = data_wrangling_state
    )
  )
}
