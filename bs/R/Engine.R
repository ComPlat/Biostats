backend_result_state <- R6::R6Class(
  "backend_result_state",
  public = list(
    curr_data = NULL, curr_name = NULL,
    all_data = list(), all_names = list(),
    history = list(),
    counter = 0,
    initialize = function() {}
  )
)

backend_data_model_state <- R6::R6Class(
  "backend_data_model_state",
  public = list(
    df = NULL,
    formula = NULL,
    backup_df = NULL,
    filter_col = NULL,
    filter_group = NULL,
    initialize = function(df) {
      self$df <- df
    }
  )
)

backend_data_wrangling_state <- R6::R6Class(
  "backend_data_wrangling_state",
  public = list(
    df = NULL,
    df_name = "df",
    current_page = 1,
    total_pages = 1,
    counter_id = 0,
    intermediate_vars = list(),
    initialize = function(backend_data_model_state) {
      self$df_name <- create_df_name(self$df_name, names(backend_data_model_state$df))
      self$df <- backend_data_model_state$df
    }
  )
)

communicator <- R6::R6Class(
  "communicator",
  public = list(
    print_warn = NULL,
    print_err = NULL,
    # check and print notifications
    print_req = NULL,
    # print notification without check
    print_noti = NULL,
    print_success = NULL,
    initialize = function() {
      self$print_warn <- print_warn
      self$print_err <- print_err
      self$print_noti <- print_noti
      self$print_req <- print_req
      self$print_success <- print_success
    }
  )
)
backend_communicator <- R6::R6Class(
  "backend_communicator",
  public = list(
    print_warn = function(msg) warning(msg),
    print_err = function(msg) stop(msg),
    print_req = function(cond, msg) if (!cond) stop(msg),
    print_noti = function(msg) message(msg),
    print_success = function(msg) message(msg),
    initialize = function() {}
  )
)

correlation <- R6::R6Class(
  "correlation",
  public = list(
    com = NULL,
    df = NULL,
    formula = NULL,
    dep = NULL,
    indep = NULL,
    method = NULL,
    alternative = NULL,
    conflevel = NULL,
    initialize = function(df, formula, method, alternative, conflevel, com = communicator) {
      self$df <- df
      self$formula <- formula
      formula <- as.character(formula)
      self$dep <- formula[2]
      self$indep <- formula[3]
      self$method <- method
      self$alternative <- alternative
      self$conflevel <- conflevel
      self$com <- com$new()
    },
    validate = function() {
      check_ast(str2lang(self$indep), colnames(self$df))
      check_ast(str2lang(self$dep), colnames(self$df))
    },
    eval = function(ResultsState) {
      fit <- NULL
      withCallingHandlers(
        expr = {
          fit <- broom::tidy(
            cor.test(
              self$df[, self$dep], self$df[, self$indep],
              method = self$method,
              alternative = self$alternative,
              conf.level = self$conflevel
            ))
          ResultsState$counter <- ResultsState$counter + 1
          new_name <- paste0(
            ResultsState$counter, " Correlation ", firstup(self$method)
          )
          self$create_history(new_name, ResultsState)
          ResultsState$all_data[[new_name]] <- fit
          check_rls(ResultsState$all_data, fit)
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      return(fit)
    },
    create_history = function(new_name, ResultsState) {
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "Correlation",
        formula = deparse(self$formula),
        "Correlation method" = self$method,
        "Alternative hypothesis" = self$alternative,
        "Confidence level of the interval" = self$conflevel,
        "Result name" = new_name
      )
    }
  )
)

visualisation <- R6::R6Class(
  "visualisation",
  public = list(
    com = NULL,
    df = NULL,
    x = NULL,
    y = NULL,
    method = NULL,
    xlabel = NULL,
    type_of_x = NULL,
    ylabel = NULL,
    colour_var = NULL,
    colour_legend_title = NULL,
    colour_theme = NULL,
    fill_var = NULL,
    fill_legend_title = NULL,
    fill_theme = NULL,
    facet_mode = NULL,
    facet_var = NULL,
    facet_y_scaling = NULL,
    xrange = NULL,
    yrange = NULL,
    width = NULL,
    height = NULL,
    resolution = NULL,
    col_names = NULL,
    fit_method = "none", # NOTE: currently this is not offered to the user

    initialize = function(df, x, y, method, xlabel, type_of_x, ylabel,
                          colour_var, colour_legend_title, colour_theme,
                          fill_var, fill_legend_title, fill_theme,
                          facet_var, facet_y_scaling,
                          xrange, yrange, width, height, resolution,
                          com = communicator) {
      self$com <- com$new()

      # User input
      self$df <- df
      self$x <- x
      self$y <- y
      self$method <- method
      self$xlabel <- xlabel
      self$type_of_x <- type_of_x
      self$ylabel <- ylabel
      self$colour_var <- colour_var
      self$colour_legend_title <- colour_legend_title
      self$colour_theme <- colour_theme
      self$fill_var <- fill_var
      self$fill_legend_title <- fill_legend_title
      self$fill_theme <- fill_theme
      self$facet_var <- facet_var
      self$facet_y_scaling <- facet_y_scaling
      self$xrange <- xrange
      self$yrange <- yrange
      self$width <- width
      self$height <- height
      self$resolution <- resolution
      # Additional information based on user input
      self$facet_mode <- "none"
      if (self$facet_var != "") {
        self$facet_mode <- "facet_wrap"
      }

      self$col_names <- names(self$df)

      if (self$type_of_x == "numeric") {
        self$df[, x] <- as.numeric(self$df[, x])
      }
    },
    validate = function() {
      # Run first checks:
      if (self$width < 0) {
        self$com$print_warn("width has to be a positive number; It is set to 10 cm")
        self$width <- 10
      }
      if (self$height < 0) {
        self$com$print_warn("height has to be a positive number; It is set to 10 cm")
        self$height <- 10
      }
      if (self$width > 100) {
        self$com$print_warn("width exceeds max value of 100 cm; It is changed to 100 cm")
        self$width <- 100
      }
      if (self$height > 100) {
        self$com$print_warn("height exceeds max value of 100 cm; It is changed to 100 cm")
        self$height <- 100
      }
    },
    eval = function(ResultsState) {
      p <- tryCatch(
        {
          withCallingHandlers(
            {
              if (self$method == "box") {
                p <- BoxplotFct(
                  self$df, self$x, self$y, self$xlabel, self$ylabel,
                  self$fill_var, self$fill_legend_title, self$fill_theme,
                  self$colour_var, self$colour_legend_title, self$colour_theme,
                  self$facet_mode, self$facet_var, self$facet_y_scaling,
                  self$xrange[1], self$xrange[2], self$yrange[1], self$yrange[2]
                )
              } else if (self$method == "dot") {
                p <- DotplotFct(
                  self$df, self$x, self$y, self$xlabel, self$ylabel,
                  self$fit_method,
                  self$colour_var, self$colour_legend_title, self$colour_theme,
                  self$facet_mode, self$facet_var, self$facet_y_scaling,
                  k = NULL,
                  self$xrange[1], self$xrange[2], self$yrange[1], self$yrange[2]
                )
              } else if (self$method == "line") {
                p <- LineplotFct(
                  self$df, self$x, self$y, self$xlabel, self$ylabel,
                  self$colour_var, self$colour_legend_title, self$colour_theme,
                  self$facet_mode, self$facet_var, self$facet_y_scaling,
                  self$xrange[1], self$xrange[2], self$yrange[1], self$yrange[2]
                )
              }
            },
            warning = function(warn) {
              self$com$print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
          check_rls(ResultsState$all_data, p)
          ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
          ResultsState$counter <- ResultsState$counter + 1
          new_result_name <- paste0(
            ResultsState$counter, " Visualization ", c(box = "Boxplot", dot = "Scatterplot", line = "Lineplot")[self$method]
          )
          ResultsState$all_data[[new_result_name]] <- new("plot", p = p, width = self$width, height = self$height, resolution = self$resolution)
          self$create_history(new_result_name, ResultsState)
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          return(p)
        },
        error = function(err) {
          self$com$print_err(paste("An error occurred: ", conditionMessage(err)))
          stop(conditionMessage(err))
        }
      )
      return(p)
    },
    create_history = function(new_result_name, ResultsState) {
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "Visualisation",
        x = self$x, y = self$y, "Plot-type" = self$method,
        "X axis label" = self$xlabel, "Y axis label" = self$ylabel, "Type of x" = self$type_of_x,
        "Colour variable" = self$colour_var, "Legend title for colour" = self$colour_legend_title,
        "Colour theme" = self$colour_theme,
        "Fill variable" = self$fill_var, "Legend title for fill" = self$fill_legend_title,
        "Fill theme" = self$fill_theme,
        "Split in subplots" = self$facet_mode, "Split by" = self$facet_var,
        "How to scale y in subplots" = self$facet_y_scaling,
        "X-Range" = as.character(self$xrange),
        "Y-Range" = as.character(self$yrange),
        Width = self$width, Height = self$height, Resolution = self$resolution,
        "Result name" = new_result_name
      )
    }
  )
)

apply_filter <- R6::R6Class(
  "apply_filter",
  public = list(
    selected_cols = NULL,
    selected_groups = NULL,
    com = NULL,
    initialize = function(selected_cols, selected_groups, com = communicator) {
      self$selected_cols <- selected_cols
      self$selected_groups <- selected_groups

      self$com <- com$new()
    },
    validate = function() {
      try(
        {
          if (length(self$selected_groups) == 0 || length(self$selected_cols) == 0) {
            stop("Invalid subset either no columns or now levels of the columns were selected")
          }
        },
        silent = TRUE
      )
    },
    eval = function(DataModelState, ResultsState) {
      DataModelState$backup_df <- DataModelState$df
      DataModelState$df <- split(DataModelState$df, self$selected_cols, self$selected_groups)
      DataModelState$filter_col <- self$selected_cols
      DataModelState$filter_group <- self$selected_groups
      self$create_history(ResultsState)
    },
    create_history = function(ResultsState) {
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "ApplyFilter",
        Variable = paste(self$selected_cols, collapse = ", "),
        "Variable levels" = paste(self$selected_groups, collapse = ", ")
      )
    }
  )
)
remove_filter <- R6::R6Class(
  "remove_filter",
  public = list(
    initialize = function() {},
    validate = function() {},
    eval = function(ResultsState, DataModelState) {
      DataModelState$df <- DataModelState$backup_df
      DataModelState$backup_df <- NULL
      DataModelState$filter_col <- NULL
      DataModelState$filter_group <- NULL
      self$create_history(ResultsState, DataModelState)
    },
    create_history = function(ResultsState, DataModelState) {
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "RemoveFilter",
        Variable = paste(DataModelState$filter_col, collapse = ", "),
        "Variable levels" = paste(DataModelState$filter_group, collapse = ", ")
      )
    }
  )
)

create_intermediate_var <- R6::R6Class(
  "create_intermediate_var",
  public = list(
    df = NULL,
    df_name = NULL,
    intermediate_vars = NULL,
    operation = NULL,
    name = NULL,
    com = NULL,

    var_name = NULL,

    initialize = function(df, df_name, intermediate_vars, operation, name, com = communicator) {
      self$df <- df
      self$df_name <- df_name
      self$intermediate_vars <- intermediate_vars
      self$operation <- operation
      self$name <- name
      self$com <- com$new()
    },

    validate = function() {
      self$com$print_req(self$name != "", "Please set a name for the intermediate variable")

      # Check the new name of the intermediate variable
      var_name <- self$name |> make.names()
      already_used_names <- names(self$df)
      if (length(self$intermediate_vars) >= 1) {
        already_used_names <- c(already_used_names, names(self$intermediate_vars))
      }
      if (var_name %in% already_used_names) {
        self$com$print_err("Found invalid variable name as it is already used by a column of the data table, or as other intermediate variable")
        stop("Invalid variable name")
      }
      if (var_name == self$df_name) {
        self$com$print_err("Found invalid variable name df. This name is reserved for the dataset")
        stop("Invalid variable name")
      }
      self$var_name <- var_name

      # Can string be converted to R language object
      op <- try(str2lang(self$operation), silent = TRUE)
      if (inherits(op, "try-error")) {
        self$com$print_err("Could not convert operation to R code")
        self$com$print_err(op)
        stop("Invalid code")
      }

      # Check the ast and check that all variables used are found
      e <- try({
        check_length_code(self$operation)
        vars <- c(self$df_name, names(self$df))
        if (length(self$intermediate_vars) >= 1) {
          vars <- c(vars, names(self$intermediate_vars))
        }
        check_ast(op, vars)
      })
      if (inherits(e, "try-error")) {
        self$com$print_err(e)
        stop("Invalid ast")
      }
    },

    eval = function(ResultsState, DataWranglingState) {
      e <- try({
        eval_env <- new.env()
        list2env(self$intermediate_vars, envir = eval_env)
        list2env(self$df, envir = eval_env) # NOTE: this adds each column as own variable
        new <- eval(parse(text =self$operation), envir = eval_env)
        check_type_res(new)
        check_rls(ResultsState$all_data, new)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create intermediate variable")
      } else {
        DataWranglingState$intermediate_vars[[self$var_name]] <- new
        ResultsState$counter <- ResultsState$counter + 1

        # NOTE: for better saving the results
        new_df <- setNames(data.frame(new), self$name)
        ResultsState$all_data[[paste0(self$var_name, ResultsState$counter)]] <- new_df
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateIntermediateVariable",
          operation = self$operation,
          name = self$name
        )
      }
    }
  )
)
remove_intermediate_var <- R6::R6Class(
  "remove_intermediate_var",
  public = list(
    name = NULL,
    com = NULL,

    initialize = function(name, com = communicator) {
      self$name <- name
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState, DataWranglingState) {
      if (!is.null(DataWranglingState$intermediate_vars[[self$name]])) {
        DataWranglingState$intermediate_vars[[self$name]] <- NULL
        self$com$print_success(paste("Removed intermediate result:", self$name))
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "RemoveIntermediateVariable",
          "Intermediate variable" = self$name
        )
      }
    }
  )
)
create_new_col <- R6::R6Class(
  "create_new_col",
  public = list(
    df = NULL,
    df_name = NULL,
    intermediate_vars = NULL,
    operation = NULL,
    name = NULL,
    com = NULL,

    initialize = function(df, df_name, intermediate_vars, operation, name, com = communicator) {
      self$df <- df
      self$df_name <- df_name
      self$intermediate_vars <- intermediate_vars
      self$operation <- operation
      self$name <- name
      self$com <- com$new()
    },

    validate = function() {
      self$com$print_req(self$name != "", "Please set a name for the new column")
 

      # Check the new name of the column
      var_name <- self$name |> make.names()
      already_used_names <- names(self$df)
      if (length(self$intermediate_vars) >= 1) {
        already_used_names <- c(already_used_names, names(self$intermediate_vars))
      }
      if (var_name %in% already_used_names) {
        self$com$print_err("Found invalid variable name as it is already used by a column of the data table, or as other intermediate variable")
        stop("Invalid variable name")
      }
      if (var_name == self$df_name) {
        self$com$print_err("Found invalid variable name df. This name is reserved for the dataset")
        stop("Invalid variable name")
      }
      self$name <- var_name

      # Can string be converted to R language object
      op <- try(str2lang(self$operation), silent = TRUE)
      if (inherits(op, "try-error")) {
        self$com$print_err("Could not convert operation to R code")
        stop("Invalid code")
      }

      # Check the ast and check that all variables used are found
      e <- try({
        check_length_code(self$operation)
        vars <- c(self$df_name, names(self$df))
        if (length(self$intermediate_vars) >= 1) {
          vars <- c(vars, names(self$intermediate_vars))
        }
        check_ast(op, vars)
      })
      if (inherits(e, "try-error")) {
        self$com$print_err(e)
        stop("Invalid ast")
      }

    },

    eval = function(ResultsState, DataWranglingState, DataModelState) {
      e <- try({
        eval_env <- new.env()
        list2env(self$intermediate_vars, envir = eval_env)
        list2env(self$df, envir = eval_env)  # NOTE: this adds each column as own variable
        new <- eval(parse(text = self$operation), envir = eval_env)
        check_type_res(new)
        check_rls(ResultsState$all_data, new)
        self$df[, self$name] <- new
        DataWranglingState$df <- self$df
        if (!is.null(DataModelState$backup_df)) {
          eval_env <- new.env()
          list2env(self$intermediate_vars, envir = eval_env)
          list2env(DataModelState$backup_df, envir = eval_env)  # NOTE: this adds each column as own variable
          new <- eval(parse(text = self$operation), envir = eval_env)
          check_type_res(new)
          DataModelState$backup_df[, self$name] <- new
          self$com$print_warn("Conducted operation also for entire dataset and not only for the subset")
        }
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create new column")
      } else {
        DataModelState$df <- DataWranglingState$df
        DataWranglingState$counter_id <- DataWranglingState$counter_id + 1
        ResultsState$counter <- ResultsState$counter + 1
        new_name <- paste0("Dataset", ResultsState$counter)
        ResultsState$all_data[[new_name]] <- DataModelState$df
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateNewColumn",
          operation = self$operation,
          "column name" = self$name
        )
      }
    }
  )
)

create_formula <- R6::R6Class(
  "create_formula ",
  public = list(
    response_var = NULL,
    right_site = NULL,
    df = NULL,
    com = NULL,

    initialize = function(response_var, right_site, df, com = communicator) {
      self$response_var <- response_var
      self$right_site <- right_site
      self$df <- df
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(DataModelState) {
      formula <- paste(self$response_var, " ~ ", self$right_site)
      formula <- as.formula(formula)
      check_ast(formula, colnames(self$df))
      DataModelState$formula <- formula
      model <- lm(formula, data = self$df)
      extract_eq(model, wrap = TRUE)
    }
  )
)
summarise_model <- R6::R6Class(
  "summarise_model",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,
    initialize = function(df, formula, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      model <- lm(self$formula, data = self$df)
      f_split <- split_formula(self$formula)
      predictors <- vars_rhs(f_split[[2]])
      response <- all.vars(f_split$response)
      n <- 100
      new_data <- create_new_data(self$formula, self$df, predictors, n)
      new_data$fit <- predict(model, newdata = new_data)
      names(new_data)[ncol(new_data)] <- response

      # R²
      r2 <- summary(model)$r.squared
      r2_label <- sprintf("R² = %.3f", r2)

      # aes
      show_prediction <- TRUE
      aes <- NULL
      if (length(predictors) == 1) {
        aes <- aes(x = .data[[predictors]], y = .data[[response]])
      } else if (length(predictors) == 2) {
        aes <- aes(x = .data[[predictors[1]]],
          y = .data[[response]],
          colour = .data[[predictors[2]]])
      } else {
        self$com$print_warn("Cannot visualize more than two predictors. Therefore the newly predicted response data is not plotted")
        aes <- aes(x = .data[[predictors[1]]],
          y = .data[[response]], colour = .data[[predictors[2]]])
        show_prediction <- FALSE
      }
      y_max <- max(self$df[[response]], na.rm = TRUE)

      p <- ggplot(self$df, aes(!!!aes)) +
        geom_point() +
        labs(caption = r2_label)

      if (show_prediction) {
        p <- p + geom_line(data = new_data, aes(!!!aes))
      }
      ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot

      ResultsState$counter <- ResultsState$counter + 1
      new_result_name <- paste0(
        ResultsState$counter, " Model plot"
      )
      ResultsState$all_data[[new_result_name]] <-
        new("plot", p = p, width = 15, height = 15, resolution = 600)

      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "ModelSummary",
        formula = deparse(self$formula),
        R2 = r2_label,
        "Result name" = new_result_name
      )

      ResultsState$counter <- ResultsState$counter + 1
      new_result_name <- paste0(
        ResultsState$counter, " Model Summary"
      )
      ResultsState$all_data[[new_result_name]] <- broom::tidy(model)
    }

  )
)


shapiro_on_data <- R6::R6Class(
  "shapiro_on_data",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,

    initialize = function(df, formula, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$com = com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      res <- list()
      res <- withCallingHandlers(
        {
          dat <- splitData(self$df, self$formula)
          for (i in unique(dat[, 2])) {
            tempDat <- dat[dat[, 2] == i, ]
            temp <- broom::tidy(shapiro.test(tempDat[, 1]))
            if (!is.null(temp)) {
              temp$variable <- i
              temp$`Normal distributed` <- temp$p.value > 0.05
              res[[length(res) + 1]] <- temp
            }
          }
          res <- do.call(rbind, res)
          check_rls(ResultsState$all_data, res)
          res
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      ResultsState$counter <- ResultsState$counter + 1
      new_name <- paste0(
        "ShapiroDataNr", ResultsState$counter
      )
      ResultsState$all_data[[new_name]] <-res
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "ShapiroOnData",
        formula = deparse(self$formula),
        "Result name" = new_name
      )
      return(res)
    }
  )
)
shapiro_on_residuals <- R6::R6Class(
  "shapiro_on_residuals",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,

    initialize = function(df, formula, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      res <- NULL
      withCallingHandlers(
        {
          fit <- lm(self$formula, data = self$df)
          r <- resid(fit)
          res <- broom::tidy(shapiro.test(r))
          res$`Residuals normal distributed` <- res$p.value > 0.05
          check_rls(ResultsState$all_data, res)
          res
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      ResultsState$counter <- ResultsState$counter + 1
      new_name <- paste0(
        "ShaprioResidualsNr", ResultsState$counter
      )
      ResultsState$all_data[[new_name]] <- res
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "ShapiroOnResiduals",
        formula = deparse(self$formula),
        "Result name" = new_name
      )
      return(res)
    }
  )
)
levene <- R6::R6Class(
  "levene",
  public = list(
    df = NULL,
    formula = NULL,
    center = NULL,
    com = NULL,

    initialize = function(df, formula, center, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$center <- center
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      res <- NULL
      withCallingHandlers(
        {
          res <- broom::tidy(
            car::leveneTest(self$formula, data = self$df, center = self$center)
          )
          res$`Variance homogenity` <- res$p.value > 0.05
          check_rls(ResultsState$all_data, res)
          res
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      ResultsState$counter <- ResultsState$counter + 1
      new_name <- paste0(
        "LeveneTestNr", ResultsState$counter
      )
      ResultsState$all_data[[new_name]] <- res
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "LeveneTest",
        formula = deparse(self$formula),
        "Data center" = self$center,
        "Result name" = new_name
      )
      return(res)
    }
  )
)
diagnostic_plots <- R6::R6Class(
  "diagnostic_plots",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,

    initialize = function(df, formula, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      p <- NULL
      withCallingHandlers(
        {
          p <- diagnosticPlots(self$df, self$formula)
          check_rls(ResultsState$all_data, p)
          p
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      ResultsState$counter <- ResultsState$counter + 1
      new_result_name <- paste0("DiagnosticPlotNr", ResultsState$counter)
      ResultsState$all_data[[new_result_name]] <-
        new("plot", p = p, width = 15, height = 15, resolution = 600)
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "DiagnosticPlots",
        formula = deparse(self$formula),
        "Result name" = new_result_name
      )
      return(p)
    }
  )
)

dose_response <- R6::R6Class(
  "dose_response",
  public = list(
    df = NULL,
    outliers = NULL,
    is_xlog = NULL,
    is_ylog = NULL,
    substance_names = NULL,
    formula = NULL,
    com = NULL,
    res_df = NULL,
    res_p = NULL,

    initialize = function(df, outliers,
                          is_xlog, is_ylog,
                          substance_names,
                          formula, com = communicator) {
      self$df <- df
      self$outliers <- outliers
      self$is_xlog <- is_xlog
      self$is_ylog <- is_ylog
      self$substance_names <- substance_names
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {},

    set_result = function(ResultsState, res_df, res_p) {
      # NOTE: this is only the case if history is replayed
      # To keep the structure of the complex dose response state
        new_result_name <- ""
        if (is.null(self$outliers)) { # Running the entire analysis
          ResultsState$curr_data <- new("doseResponse", df = res_df, p = res_p, outlier_info = "")
          ResultsState$curr_name <- paste(
            "Test Nr", length(ResultsState$all_names) + 1,
            "Conducted dose response analysis"
          )
          ResultsState$counter <- ResultsState$counter + 1
          new_result_name <- paste0("DoseResponseNr", ResultsState$counter)
          ResultsState$all_data[[new_result_name]] <- new(
            "doseResponse",
            df = res_df, p = res_p, outlier_info = ""
          )
        } else { # Rerun with outliers
          ResultsState$curr_data <- new(
            "doseResponse",
            df = res_df, p = res_p, outlier_info = create_outlier_info(self$outliers)
          )
          ResultsState$curr_name <- paste(
            "Test Nr", length(ResultsState$all_names) + 1,
            "Conducted dose response analysis"
          )
          ResultsState$counter <- ResultsState$counter + 1
          new_result_name <- paste0("DoseResponseNr", ResultsState$counter)
          ResultsState$all_data[[new_result_name]] <- new(
            "doseResponse",
            df = res_df, p = res_p, outlier_info = create_outlier_info(self$outliers)
          )
        }
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "DoseResponse",
          "Column containing the names" = self$substance_names,
          "Log transform x-axis" = self$is_xlog,
          "Log transform y-axis" = self$is_ylog,
          "formula" = deparse(self$formula),
          outliers = create_outlier_info(self$outliers),
          "Result name" = new_result_name
        )
    },

    eval = function(ResultsState) {
      f <- as.character(self$formula)
      dep <- f[2]
      indep <- f[3]
      res_p <- NULL
      res_df <- NULL
      err <- try({
        check_formula(self$formula)
        check_ast(str2lang(indep), colnames(self$df))
        check_ast(str2lang(dep), colnames(self$df))
        res <- ic50(
          self$df, dep,
          indep, self$substance_names, self$outliers,
          self$is_xlog, self$is_ylog
        )
        if (inherits(res, "errorClass")) {
          stop(res$error_message)
        }
        res_df <- lapply(res, function(x) {
          if (inherits(x, "errorClass")) {
            return(NULL)
          }
          return(x[[1]])
        })
        res_df <- res_df[!is.null(res_df)]
        res_df <- res_df[!sapply(res_df, is.null)]
        res_df <- Reduce(rbind, res_df)
        res_p <- lapply(res, function(x) {
          if (inherits(x, "errorClass")) {
            return(NULL)
          }
          return(x[[2]])
        })
        res_p <- res_p[!is.null(res_p)]
        res_p <- res_p[!sapply(res_p, is.null)]
      })
      if (inherits(err, "try-error")) {
        self$com$print(err$message)
        return(err)
      }
      if (!is.null(ResultsState)) {
        self$set_result(ResultsState, res_df, res_p)
      }

      return(list(res_df, res_p))
    }

  )
)

t_test <- R6::R6Class(
  "t_test",
  public = list(
    df = NULL,
    formula = NULL,
    variances_equal = NULL,
    conf_level = NULL,
    alternative_hyp = NULL,
    com = NULL,

    initialize = function(df, formula,
                          variances_equal, conf_level,
                          alternative_hyp, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$variances_equal <- variances_equal
      self$conf_level <- conf_level
      self$alternative_hyp <- alternative_hyp
      self$com = com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      fit <- NULL
      withCallingHandlers(
        {
          eq <- TRUE
          if (self$variances_equal == "noeq") {
            eq <- FALSE
          }
          fit <- broom::tidy(t.test(self$formula,
            data = self$df, conf.level = self$conf_level,
            alternative = self$alternative_hyp, var.equal = eq
          ))
          check_rls(ResultsState$all_data, fit)
          fit
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      ResultsState$counter <- ResultsState$counter + 1
      new_name <- paste0(
        "TTestNr", ResultsState$counter
      )
      ResultsState$all_data[[new_name]] <- fit
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "TTest",
        formula = deparse(self$formula),
        "Confidence level of the interval" = self$conf_level,
        "alternative hypothesis" = self$alternative_hyp,
        "The two variances are" = self$variances_equal,
        "Result name" = new_name
      )
      return(fit)
    }
  )
)

statistical_tests <- R6::R6Class(
  "statistical_tests",
  public = list(
    df = NULL,
    formula = NULL,
    balanced_design = NULL,
    p_val = NULL,
    p_val_adj_method = NULL,
    dep = NULL,
    indep = NULL,
    com = NULL,

    initialize = function(df, formula, balanced_design, p_val, p_val_adj_method, com = communicator) {
      self$df <- df
      self$formula <- formula
      self$balanced_design <- balanced_design
      self$p_val <- p_val
      self$p_val_adj_method <- p_val_adj_method
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState, method) {
      e <- try({
        self$indep <- as.character(self$formula)[3]
        self$dep <- as.character(self$formula)[2]
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        err <- paste0(err, "\n", "Could not use Formula")
        self$com$print_err(err)
        stop(err)
      }

      fit <- NULL
      history_data <- NULL
      withCallingHandlers(
        {
          switch(method,
            aov = {
              fit <- broom::tidy(aov(
                self$formula,
                data = self$df
              ))
              history_data <- list(type = "ANOVA", formula = deparse(self$formula))
            },
            kruskal = {
              fit <- broom::tidy(
                kruskal.test(self$formula, data = self$df)
              ) # Keep here the restriction for respone ~ predictor
              history_data <- list(type = "Kruskal-Wallis Test", formula = deparse(self$formula))
            },
            HSD = {
              check_formula(self$formula)
              aov_res <- aov(self$formula, data = self$df)
              bal <- self$balanced_design
              req(bal)
              if (bal == "Balanced") {
                bal <- TRUE
              } else {
                bal <- FALSE
              }
              fit <- agricolae::HSD.test(aov_res,
                trt = self$indep,
                alpha = self$p_val, group = TRUE, unbalanced = bal
              )$groups
              history_data <- list(type = "Tukey HSD",
                formula = deparse(self$formula), "Balanced design" = bal,
                "P-value" = self$p_val)
            },
            kruskalTest = {
              check_formula(self$formula)
              fit <- with(self$df, kruskal(self$df[, self$dep], self$df[, self$indep]),
                alpha = self$p_val, p.adj = self$p_val_adj_method, group = TRUE
              )$groups
              names(fit)[1] <- self$dep
              history_data <- list(type = "Kruskal Wallis post hoc test",
                formula = deparse(self$formula),
                "Adjusted p value method" = self$p_val_adj_method,
                "P-value" = self$p_val)
            },
            LSD = {
              check_formula(self$formula)
              aov_res <- aov(self$formula, data = self$df)
              fit <- agricolae::LSD.test(aov_res,
                trt = self$indep,
                alpha = self$p_val, p.adj = self$p_val_adj_method, group = TRUE
              )$groups
              history_data <- list(type = "Least significant difference test",
                formula = deparse(self$formula),
                "Adjusted p value method" = self$p_val_adj_method,
                "P-value" = self$p_val)
            },
            scheffe = {
              check_formula(self$formula)
              aov_res <- aov(self$formula, data = self$df)
              fit <- agricolae::scheffe.test(
                aov_res,
                trt = self$indep, alpha = self$p_val, group = TRUE
              )$groups
              history_data <- list(type = "Scheffe post hoc test",
                formula = deparse(self$formula),
                "P-value" = self$p_val)
            },
            REGW = {
              check_formula(self$formula)
              aov_res <- aov(self$formula, data = self$df)
              fit <- agricolae::REGW.test(
                aov_res,
                trt = self$indep, alpha = self$p_val, group = TRUE
              )$groups
              history_data <- list(type = "REGW post hoc test",
                formula = deparse(self$formula),
                "P-value" = self$p_val)
            }
          )
          check_rls(ResultsState$all_data, fit)

          fit <- cbind(fit, row.names(fit))
          names(fit)[ncol(fit)] <- paste0(self$indep, collapse = ".")

          fit
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )

      ResultsState$counter <- ResultsState$counter + 1
      new_name <- paste0(
        "Test_", method, "Nr", ResultsState$counter
      )
      ResultsState$all_data[[new_name]] <- fit
      ResultsState$history[[length(ResultsState$history) + 1]] <- c(
        history_data,
        "Result name" = new_name
      )

      return(fit)
    }
  )
)

remove_result <- R6::R6Class(
  "remove_result",
  public = list(
    name = NULL,
    initialize = function(name) {
      self$name <- name
    },
    validate = function() {},
    eval = function(ResultsState) {
      current_list <- ResultsState$all_data
      current_list[[self$name]] <- NULL
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "RemoveResult", "Entry removed" = self$name
      )
      ResultsState$all_data <- current_list
    }
  )
)
