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
      self$formula <- formula |> as.character()
      self$dep <- self$formula[2]
      self$indep <- self$formula[3]
      self$method <- method
      self$alternative <- alternative
      self$conflevel <- conflevel
      self$com <- com$new()
    },
    validate = function() {
      check_ast(str2lang(self$indep), colnames(self$df))
      check_ast(str2lang(self$dep), colnames(self$df))
    },
    eval = function() {
      withCallingHandlers(
        expr = broom::tidy(
          cor.test(
            self$df[, self$dep], self$df[, self$indep],
            method = self$method,
            alternative = self$alternative,
            conf.level = self$conflevel
          )
        ),
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    create_history = function(new_name, listResults) {
      listResults$history[[length(listResults$history) + 1]] <- list(
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
    eval = function(listResults) {
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
          check_rls(listResults$all_data, p)
          ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
          p
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
    },
    create_history = function(new_result_name, listResults) {
      listResults$history[[length(listResults$history) + 1]] <- list(
        type = "Visualisation",
        x = self$x, y = self$y, "Plot-type" = self$method,
        "X axis label" = self$xlabel, "Y axis label" = self$ylabel, "Type of x" = self$type_of_x,
        "Colour variable" = self$colour_var, "Legend title for colour" = self$colour_legend_title,
        "Colour theme" = self$colour_theme,
        "Fill variable" = self$fill_var, "Legend title for fill" = self$fill_legend_title,
        "Fill theme" = self$fill_theme,
        "Split in subplots" = self$facet_mode, "Split by" = self$facet_var,
        "How to scale y in subplots" = self$facet_y_scaling,
        "X-Range" = as.character(c(self$xrange)),
        "Y-Range" = as.character(c(self$yrange)),
        Width = self$width, Height = self$height, Resolution = self$resolution,
        "Result name" = new_result_name
      )
    }
  )
)


# vis <- visualisation$new(
#   df = CO2, x = "conc", y = "uptake", method = "box",
#   xlabel = "x label", type_of_x = "factor", ylabel = "y label",
#   colour_var = "", colour_legend_title = "Title colour", colour_theme = "Accent",
#   fill_var = "", fill_legend_title = "Title fill", fill_theme = "BuGn",
#   facet_var = "", facet_y_scaling = "free",
#   xrange = c(47.5, 1250.0), yrange = c(7.315, 47.775),
#   width = 10, height = 10, resolution = 300,
#   com = backend_communicator
# )
# listResults <- new.env()
# listResults$all_data <- list()
# p <- vis$eval(listResults)
# p
# .traceback()


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
    eval = function(data) {
      data$backup_df <- data$df
      data$df <- split(data$df, self$selected_cols, self$selected_groups)
      data$filter_col <- self$selected_cols
      data$filter_group <- self$selected_groups
    },
    create_history = function(listResults) {
      listResults$history[[length(listResults$history) + 1]] <- list(
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
    eval = function(dataSet) {
      dataSet$df <- dataSet$backup_df
      dataSet$backup_df <- NULL
      dataSet$filter_col <- NULL
      dataSet$filter_group <- NULL
    },
    create_history = function(listResults, dataSet) {
      listResults$history[[length(listResults$history) + 1]] <- list(
        type = "RemoveFilter",
        Variable = paste(dataSet$filter_col, collapse = ", "),
        "Variable levels" = paste(dataSet$filter_group, collapse = ", ")
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

    eval = function(listResults, r_vals) {
      e <- try({
        eval_env <- new.env()
        list2env(self$intermediate_vars, envir = eval_env)
        list2env(self$df, envir = eval_env) # NOTE: this adds each column as own variable
        new <- eval(parse(text =self$operation), envir = eval_env)
        check_type_res(new)
        check_rls(listResults$all_data, new)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create intermediate variable")
      } else {
        r_vals$intermediate_vars[[self$var_name]] <- new
        exportTestValues(
          iv_list = r_vals$intermediate_vars
        )
        listResults$counter <- listResults$counter + 1
        listResults$all_data[[paste0(self$var_name, listResults$counter)]] <- new
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "CreateIntermediateVariable",
          operation = self$operation,
          name = self$var_name
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

    eval = function(listResults, r_vals) {
      if (!is.null(r_vals$intermediate_vars[[self$name]])) {
        r_vals$intermediate_vars[[self$name]] <- NULL
        self$com$print_success(paste("Removed intermediate result:", self$name))
        listResults$history[[length(listResults$history) + 1]] <- list(
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

    eval = function(listResults, r_vals, data) {
      e <- try({
        eval_env <- new.env()
        list2env(self$intermediate_vars, envir = eval_env)
        list2env(self$df, envir = eval_env)  # NOTE: this adds each column as own variable
        new <- eval(parse(text = self$operation), envir = eval_env)
        check_type_res(new)
        check_rls(listResults$all_data, new)
        self$df[, self$name] <- new
        r_vals$df <- self$df
        if (!is.null(data$backup_df)) {
          eval_env <- new.env()
          list2env(self$intermediate_vars, envir = eval_env)
          list2env(data$backup_df, envir = eval_env)  # NOTE: this adds each column as own variable
          new <- eval(parse(text = self$operation), envir = eval_env)
          check_type_res(new)
          data$backup_df[, self$name] <- new
          self$com$print_warn("Conducted operation also for entire dataset and not only for the subset")
        }
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create new column")
      } else {
        data$df <- r_vals$df
        r_vals$counter_id <- r_vals$counter_id + 1
        listResults$counter <- listResults$counter + 1
        new_name <- paste0("Dataset", listResults$counter)
        listResults$all_data[[new_name]] <- data$df
        listResults$history[[length(listResults$history) + 1]] <- list(
          type = "CreateNewColumn",
          operation = self$operation,
          "column name" = new_name
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

    eval = function(data) {
      formula <- paste(self$response_var, " ~ ", self$right_site)
      formula <- as.formula(formula)
      check_ast(formula, colnames(self$df))
      data$formula <- formula
      model <- lm(formula, data = self$df)
      extract_eq(model, wrap = TRUE)
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

    eval = function(listResults) {
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
          check_rls(listResults$all_data, res)
          res
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
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

    eval = function(listResults) {
      res <- NULL
      withCallingHandlers(
        {
          fit <- lm(self$formula, data = self$df)
          r <- resid(fit)
          res <- broom::tidy(shapiro.test(r))
          res$`Residuals normal distributed` <- res$p.value > 0.05
          check_rls(listResults$all_data, res)
          res
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
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

    eval = function(listResults) {
      res <- NULL
      withCallingHandlers(
        {
          res <- broom::tidy(
            car::leveneTest(self$formula, data = self$df, center = self$center)
          )
          res$`Variance homogenity` <- res$p.value > 0.05
          check_rls(listResults$all_data, res)
          res
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
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

    eval = function(listResults) {
      p <- NULL
      withCallingHandlers(
        {
          p <- diagnosticPlots(self$df, self$formula)
          check_rls(listResults$all_data, p)
          p
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
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

    eval = function() {
      f <- as.character(self$formula)
      dep <- f[2]
      indep <- f[3]
      resDF <- NULL
      resP <- NULL
      err <- try({
        check_formula(self$formula)
        check_ast(str2lang(indep), colnames(self$df))
        check_ast(str2lang(dep), colnames(self$df))
        res <- ic50(
          self$df, dep,
          indep, self$names, self$outliers,
          self$is_xlog, self$is_ylog
        )
        if (inherits(res, "errorClass")) {
          stop(res$error_message)
        }
        resDF <- lapply(res, function(x) {
          if (inherits(x, "errorClass")) {
            return(NULL)
          }
          return(x[[1]])
        })
        resDF <- resDF[!is.null(resDF)]
        resDF <- resDF[!sapply(resDF, is.null)]
        resDF <- Reduce(rbind, resDF)
        resP <- lapply(res, function(x) {
          if (inherits(x, "errorClass")) {
            return(NULL)
          }
          return(x[[2]])
        })
        resP <- resP[!is.null(resP)]
        resP <- resP[!sapply(resP, is.null)]
      })
      if (inherits(err, "try-error")) {
        return(err)
      }
      return(list(resDF, resP))
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

    eval = function(listResults) {
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
          check_rls(listResults$all_data, fit)
          fit
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
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

    eval = function(listResults, method) {
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
          check_rls(listResults$all_data, fit)

          fit <- cbind(fit, row.names(fit))
          names(fit)[ncol(fit)] <- paste0(self$indep, collapse = ".")

          fit
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
      return(list(fit = fit, history_data = history_data))
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
    eval = function(listResults) {
      current_list <- listResults$all_data
      current_list[[self$name]] <- NULL
      listResults$history[[length(listResults$history) + 1]] <- list(
        type = "RemoveResult", "Entry removed" = self$name
      )
      listResults$all_data <- current_list
    }
  )
)

parse_history_json <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}
recreate_class <- function(entry, df, intermediate_vars, com = backend_communicator) {
  switch(entry$type,
    "CreateNewColumn" = create_new_col$new(
      df = df,
      df_name = "df",
      intermediate_vars = intermediate_vars,
      operation = entry$operation,
      name = entry[["column name"]],
      com = com
    ),
    "CreateFormula" = create_formula$new(
      response_var = str2lang(entry$formula)[[2]] |> as.character(),
      right_site = str2lang(entry$formula)[[3]] |> as.character(),
      df = df,
      com = com
    ),
    "shapiroOnResiduals" = shapiro_on_residuals$new(
      df = df,
      formula = as.formula(entry$formula),
      com = com
    ),
    "DiagnosticPlots" = diagnostic_plots$new(
      df = df,
      formula = as.formula(entry$formula),
      com = com
    ),
    "ANOVA" = statistical_tests$new(
      df = df,
      formula = as.formula(entry$formula),
      balanced_design = "Balanced",
      p_val = 0.05,
      p_val_adj_method = "none",
      com = com
    ),
    stop(paste("Unknown type:", entry$type))
  )
}
replay_history <- function(path, df, listResults, r_vals) {
  history <- parse_history_json(path)
  for (entry in history) {
    obj <- recreate_class(entry, df, r_vals$intermediate_vars)
    obj$validate()
    obj$eval(listResults, r_vals)
  }
}

