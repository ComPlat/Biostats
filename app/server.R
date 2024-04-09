options(shiny.port = 3838)
library(shiny)
library(DT)
library(httr)
library(agricolae)
library(ggplot2)
library(broom)
library(readxl)
library(openxlsx)
library(purrr)
library(RColorBrewer)
library(MTT)
library(COMELN)
library(openssl)
library(jose)
library(pheatmap)
library(png)
library(ggpmisc)
library(R6)
library(drc)
library(patchwork)

source("check_ast.R")

server <- function(input, output, session) {
  # -------------------------------------------------------------------------------------------------------
  # 1. download data from ELN and load into shiny
  # -------------------------------------------------------------------------------------------------------
  var <- reactiveValues(
    df = NULL, token = NULL,
    url = NULL, id = NULL,
    type = NULL, ip = NULL,
    filename_user = NULL
  )

  source("utils.R")
  source("server/readData.R")
  source("server/plotting.R")

  output$dat1 <- renderDT({
    readData("readData", var)
    var$df
  })

  # -------------------------------------------------------------------------------------------------------
  # 2. modify dependent variable
  # -------------------------------------------------------------------------------------------------------
  observeEvent(input$mod, {
    dat <- var$df
    op <- input$op
    new_mod <- input$new_mod
    req(op)
    req(new_mod)
    req(dat)
    new <- NULL
    err <- NULL
    e <- try({
      ast <- get_ast(str2lang(op))
      ast <- ast[[length(ast)]]
    })
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
      return()
    }
    if (ast == "Error") {
      showNotification("Unallowed function found")
    } else {
      e <- try(
        new <- with(dat, eval(parse(text = op)))
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
      }
      var$df[, new_mod] <- new
      output$dat1 <- renderDT(var$df)
      output$mod_error <- renderText(err)
    }
  })

  # -------------------------------------------------------------------------------------------------------
  # 3. Save results in file and send file
  # -------------------------------------------------------------------------------------------------------
  observeEvent(checker$check, {
    # COMELN::upload(session, "https://3.complat-eln-stage.ibcs.kit.edu", checker$file, new_name = var$filename_user)
    # "http://localhost:3000"
    COMELN::upload(session, checker$file, new_name = var$filename_user)
    checker$check <- NULL
    checker$file <- NULL
  })


  # -------------------------------------------------------------------------------------------------------
  # 4. statistical methods
  # -------------------------------------------------------------------------------------------------------
  # variable which saves result of calculation
  res <- reactiveValues(data = NULL)

  # variable which is used to store all results
  result <- reactiveValues(d = NULL, saved = NULL, names = NULL, curr_name = NULL)

  # upload result to ELN in correlation tab
  checker <- reactiveValues(check = NULL, file = NULL)

  saving <- function(indices) {
    if (length(indices) == 0) {
      showNotification("Nothing to upload")
      return(NULL)
    }

    filename <- function() {
      tempfile <- tempfile(tmpdir = "/home/shiny/results", fileext = ".xlsx")
      checker$file <- tempfile
      return(tempfile)
    }

    plot_files <- new.env()
    content <- function(file, indices) {
      l <- list()

      counter <- 1
      for (i in indices) {
        l[[counter]] <- result$saved[[i]]
        counter <- counter + 1
      }

      if (length(l) == 0) {
        return(NULL)
      }

      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")

      curr_row <- 1
      for (i in seq_along(1:length(l))) {
        curr_val <- l[[i]]
        if (class(curr_val)[1] != "plotResult") { # own R6 class
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5
        } else { # find plot element
          tempfile_plot <- tempfile(fileext = ".png")
          ggsave(tempfile_plot,
            plot = curr_val$obj,
            width = curr_val$width, height = curr_val$height,
            dpi = curr_val$dpi, device = "jpeg", units = "cm"
          )
          insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
          curr_row <- curr_row + 20
          plot_files$files <- c(plot_files, tempfile_plot)
        }
      }

      res <- tryCatch(
        expr = {
          openxlsx::saveWorkbook(wb, file)
        },
        error = function(e) {
          "Error"
        }
      )
      return(res)
    }

    fn <- filename()
    checker$file <- fn
    content(fn, indices)

    for (i in seq_along(plot_files$files)) {
      tryCatch(
        {
          system(paste("rm -r ", i))
        },
        warning = function(warn) {
          showNotification(paste("A warning occurred: ", conditionMessage(warn)), duration = 0)
        },
        error = function(err) {
          showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
        }
      )
    }


    checker$check <- TRUE
  }

  # -------------------------------------------------------------------------------------------------------
  # 4.1 correlations
  # -------------------------------------------------------------------------------------------------------
  # save results
  observeEvent(input$corr_save, {
    result$saved[[length(result$saved) + 1]] <- result$d
    result$names[[length(result$names) + 1]] <- result$curr_name
    updateCheckboxGroupInput(session, "TableSaved5", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved2", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved3", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved4", choices = result$names)
  })

  # send already saved results in file
  observeEvent(input$corr_upload, {
    indices <- match(input$TableSaved, result$names)
    var$filename_user <- input$corr_file_name
    saving(indices)
    js$closewindow()
  })

  corr_fct <- function(method) {
    dep1 <- input$indep3
    req(dep1)
    dep2 <- input$dep3
    req(dep2)
    df <- var$df
    req(df)

    fit <- NULL
    err <- NULL

    e <- try(
      fit <- broom::tidy(cor.test(df[, dep1], df[, dep2], method = method, alternative = input$alt0, conf.level = input$conflevel0))
    )
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }

    res$data <- renderTable(fit, digits = 6)
    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "Conducted test: ", method)
    output$cor_result <- renderTable(fit, digits = 6)
    output$cor_error <- renderText(err)
  }

  # pearson
  observeEvent(input$pear, {
    corr_fct("pearson")
  })

  output$cor_result <- renderTable(
    {
      res$data
    },
    digits = 6
  )

  # spearman
  observeEvent(input$spear, {
    corr_fct("spearman")
  })

  output$cor_result <- renderTable(
    {
      res$data
    },
    digits = 6
  )

  # kendall
  observeEvent(input$kendall, {
    corr_fct("kendall")
  })

  output$cor_result <- renderTable(
    {
      res$data
    },
    digits = 6
  )



  # -------------------------------------------------------------------------------------------------------
  # 5. plotting
  # -------------------------------------------------------------------------------------------------------
  plotting("plotting", var, result, res, plot_res)

  # save results
  observeEvent(input$plot_save, {
    result$saved[[length(result$saved) + 1]] <- result$d
    result$names[[length(result$names) + 1]] <- result$curr_name
    updateCheckboxGroupInput(session, "TableSaved5", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved3", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved2", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved4", choices = result$names)
  })

  # send already saved results in file
  observeEvent(input$plot_upload, {
    indices <- match(input$TableSaved2, result$names)
    var$filename_user <- input$plot_file_name
    saving(indices)
    js$closewindow()
  })


  # -------------------------------------------------------------------------------------------------------
  # 6. Assumptions
  # -------------------------------------------------------------------------------------------------------
  observeEvent(input$ass_save, {
    result$saved[[length(result$saved) + 1]] <- result$d
    result$names[[length(result$names) + 1]] <- result$curr_name
    updateCheckboxGroupInput(session, "TableSaved5", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved3", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved2", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved4", choices = result$names)
  })


  # send already saved results in file
  observeEvent(input$ass_upload, {
    indices <- match(input$TableSaved3, result$names)
    var$filename_user <- input$ass_file_name
    saving(indices)
    js$closewindow()
  })

  as_res <- reactiveValues(data = NULL)
  # -------------------------------------------------------------------------------------------------------
  # 6.1 normal distribution
  # -------------------------------------------------------------------------------------------------------
  observeEvent(input$shap, {
    correct_name <- function(name, df) {
      name %in% names(df)
    }

    change_char_input <- function(chars) {
      nams <- unlist(strsplit(chars, split = ","))
      for (i in 1:length(nams)) {
        nams[i] <- gsub(" ", "", nams[i])
      }
      nams
    }

    combine <- function(new, vec, df, first) {
      if (length(vec) == 0) {
        return(new)
      }

      if (correct_name(vec[length(vec)], df)) {
        if (isTRUE(first)) {
          new <- df[, vec[length(vec)]]
          first <- FALSE
        } else {
          new <- interaction(new, df[, vec[length(vec)]])
        }
      }

      vec <- vec[-length(vec)]
      combine(new, vec, df, first)
    }

    indep <- input$indep4
    req(indep)
    dep <- input$dep4
    req(dep)
    dat <- var$df
    req(dat)
    vec <- change_char_input(input$indep4)
    new <- rep("", dim(dat)[1])
    first <- TRUE
    new <- combine(new, vec, dat, first)

    groups <- unique(new)

    check <- TRUE
    res <- NULL
    if (length(groups) == 1) {
      if (groups == "") {
        output$as_error <- renderText("Something went wrong. Probably not found independent variable(s)")
        check <- FALSE
      }
    }

    if (isTRUE(check)) {
      res <- list()
      for (i in groups) {
        posis <- new == i
        temp_dat <- dat[posis, ]

        temp <- NULL
        err <- NULL

        e <- try(
          temp <- broom::tidy(shapiro.test(temp_dat[, dep]))
        )
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
        }
        output$as_error <- renderText(err)

        if (!is.null(temp)) {
          temp$variable <- i
          res[[length(res) + 1]] <- temp
        }
      }

      res <- do.call(rbind, res)
    }

    result$d <- res
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "normal distribution (shapiro)")

    output$as_result <- renderTable(res, digits = 6)
  })

  output$as_result <- renderTable(
    {
      as_res$data
    },
    digits = 6
  )


  # -------------------------------------------------------------------------------------------------------
  # 6.2 variance homogenity
  # -------------------------------------------------------------------------------------------------------
  as_res <- reactiveValues(data = NULL)

  observeEvent(input$leve, {
    indep <- input$indep4
    req(indep)
    dep <- input$dep4
    req(dep)

    formula <- NULL
    err <- NULL
    fit <- NULL

    e <- try(
      formula <- as.formula(paste(dep, "~", indep))
    )
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }


    if (is.null(err)) {
      df <- var$df
      req(df)

      e <- try(
        fit <- broom::tidy(car::leveneTest(formula, data = df, center = input$center))
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
      }
    }

    as_res$data <- renderTable(fit, digits = 6)
    output$as_result <- renderTable(fit, digits = 6)
    output$as_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "variance homogenity (levene)")
  })

  output$as_result <- renderTable(
    {
      as_res$data
    },
    digits = 6
  )
















  # -------------------------------------------------------------------------------------------------------
  # 7 Statistical tests
  # -------------------------------------------------------------------------------------------------------
  observeEvent(input$tests_save, {
    result$saved[[length(result$saved) + 1]] <- result$d
    result$names[[length(result$names) + 1]] <- result$curr_name
    updateCheckboxGroupInput(session, "TableSaved5", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved4", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved3", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved2", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved", choices = result$names)
  })


  # send already saved results in file
  observeEvent(input$tests_upload, {
    indices <- match(input$TableSaved4, result$names)
    var$filename_user <- input$tests_file_name
    saving(indices)
    js$closewindow()
  })

  # -------------------------------------------------------------------------------------------------------
  # 7.1 t-test
  # -------------------------------------------------------------------------------------------------------
  tests_res <- reactiveValues(data = NULL)

  observeEvent(input$ttest, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)

    formula <- NULL
    err <- NULL
    fit <- NULL

    e <- try(
      formula <- as.formula(paste(dep, "~", indep))
    )
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }

    paired <- FALSE
    if (input$paired == "p") {
      paired <- TRUE
    }

    eq <- TRUE
    if (input$vareq == "noeq") {
      eq <- FALSE
    }

    if (is.null(err)) {
      df <- var$df
      req(df)

      e <- try(
        fit <- broom::tidy(t.test(formula,
          data = df, conf.level = input$conflevel,
          alternative = input$alt, paired = paired, var.equal = eq
        ))
      )
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
      }
    }


    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "t-test or Welch-test")
  })

  output$tests_result <- renderTable(
    {
      tt_res$data
    },
    digits = 6
  )


  # -------------------------------------------------------------------------------------------------------
  # 7.2 aov
  # -------------------------------------------------------------------------------------------------------
  conduct_test <- function(method, indep, dep) {
    formula <- NULL
    err <- NULL
    fit <- NULL

    e <- try(
      formula <- as.formula(paste(dep, "~", indep))
    )
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }

    if (is.null(err)) {
      df <- var$df
      req(df)

      e <- try({
        switch(method,
          aov = {
            fit <- broom::tidy(aov(formula, data = df))
          },
          kruskal = {
            fit <- broom::tidy(kruskal.test(formula, data = df))
          },
          HSD = {
            aov_res <- aov(formula, data = df)
            bal <- input$bvsub
            req(bal)
            if (bal == "Balanced") {
              bal <- TRUE
            } else {
              bal <- FALSE
            }
            fit <- agricolae::HSD.test(aov_res,
              trt = indep,
              alpha = input$pval, group = TRUE, unbalanced = bal
            )$groups
          },
          kruskal_test = {
            fit <- with(df, kruskal(df[, dep], df[, indep]),
              alpha = input$pval, p.adj = input$padj, group = TRUE
            )$groups
          },
          LSD = {
            aov_res <- aov(formula, data = df)
            fit <- agricolae::LSD.test(aov_res,
              trt = indep,
              alpha = input$pval, p.adj = input$padj, group = TRUE
            )$groups
          },
          scheffe = {
            aov_res <- aov(formula, data = df)
            fit <- agricolae::scheffe.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
          },
          REGW = {
            aov_res <- aov(formula, data = df)
            fit <- agricolae::REGW.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
          }
        )
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        return(err)
      }
    }

    return(fit)
  }

  observeEvent(input$aov, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("aov", indep, dep)
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "Analysis of variance (AOV)")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )

  # -------------------------------------------------------------------------------------------------------
  # 7.3 kruskal wallis
  # -------------------------------------------------------------------------------------------------------
  observeEvent(input$kw, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("kruskal", indep, dep)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "Kruskal-Wallis")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )

  # -------------------------------------------------------------------------------------------------------
  # 7.4 tukey HSD
  # -------------------------------------------------------------------------------------------------------
  tests_res <- reactiveValues(data = NULL)
  observeEvent(input$tuk, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("HSD", indep, dep)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "TukeyHSD")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )

  # -------------------------------------------------------------------------------------------------------
  # 7.5 kruskal test
  # -------------------------------------------------------------------------------------------------------
  tests_res <- reactiveValues(data = NULL)
  observeEvent(input$kwposthoc, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("kruskal_test", indep, dep)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "Kurskal-Wallis PostHoc test")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )

  # -------------------------------------------------------------------------------------------------------
  # 7.6 LSD test
  # -------------------------------------------------------------------------------------------------------
  tests_res <- reactiveValues(data = NULL)
  observeEvent(input$lsd, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("LSD", indep, dep)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "LSD")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )

  # -------------------------------------------------------------------------------------------------------
  # 7.7 Scheffe test
  # -------------------------------------------------------------------------------------------------------
  tests_res <- reactiveValues(data = NULL)
  observeEvent(input$scheffe, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("scheffe", indep, dep)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "Scheffe")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )

  # -------------------------------------------------------------------------------------------------------
  # 7.8 REGW test
  # -------------------------------------------------------------------------------------------------------
  tests_res <- reactiveValues(data = NULL)
  observeEvent(input$regw, {
    indep <- input$indep6
    req(indep)
    dep <- input$dep6
    req(dep)
    fit <- conduct_test("REGW", indep, dep)
    err <- NULL
    if (inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "REGW")
  })

  output$tests_result <- renderTable(
    {
      tests_res$data
    },
    digits = 6
  )









  # -------------------------------------------------------------------------------------------------------
  # 8 calculation LC50
  # -------------------------------------------------------------------------------------------------------
  observeEvent(input$lc50_save, {
    result$saved[[length(result$saved) + 1]] <- result$d
    result$names[[length(result$names) + 1]] <- result$curr_name
    updateCheckboxGroupInput(session, "TableSaved5", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved4", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved3", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved2", choices = result$names)
    updateCheckboxGroupInput(session, "TableSaved", choices = result$names)
  })


  # send already saved results in file
  observeEvent(input$lc50_upload, {
    indices <- match(input$TableSaved5, result$names)
    var$filename_user <- input$lc50_file_name
    saving(indices)
    js$closewindow()
  })

  lc50_res <- reactiveValues(data = NULL, plot_final = NULL)

  lc50_fct <- function(plotting = FALSE) {
    df <- var$df
    req(df)
    abs <- input$abs
    req(abs)
    conc <- input$conc
    req(conc)
    names <- input$names
    req(names)
    negative_identifier <- input$negative_identifier
    req(negative_identifier)
    positive_identifier <- input$positive_identifier
    req(positive_identifier)
    res <- try(MTT::ic50(df, abs, conc, names, negative_identifier, positive_identifier))
    err <- NULL
    if (inherits(res, "try-error")) {
      err <- conditionMessage(attr(res, "condition"))
      showNotification("Error during calculation of IC50.", duration = 0)
      return()
    }

    if (is(res, "errorClass")) {
      showNotification(res$error_message, duration = 0)
      return()
    }

    data <- list()
    for (i in seq_along(res)) {
      if (is(res[[i]], "errorClass")) {
        showNotification(res[[i]]$error_message, duration = 0)
        data[[i]] <- data.frame(
          name = NA, Response_lowestdose_predicted = NA,
          Response_highestdose_predicted = NA,
          HillCoefficient = NA,
          asymptote_one = NA, asymptote_two = NA,
          IC50_relative = NA, IC50_relative_lower = NA,
          IC50_relative_higher = NA, pIC50 = NA,
          RSE = NA, p_value = NA, Problems = result[[i]][[1]]$error_message
        )
      } else {
        data[[i]] <- res[[i]][[1]]
      }
    }
    data <- do.call(rbind, data)

    if (!plotting) {
      result$d <- data
      result$curr_name <- paste("Test Nr", length(result$names) + 1, "LC50")
      lc50_res$data <- data
      return()
    }

    plots <- list()
    for (i in seq_along(res)) {
      if (is(res[[i]], "errorClass")) {
        showNotification(res[[i]]$error_message, duration = 0)
        plots[[i]] <- ggplot()
      } else {
        plots[[i]] <- res[[i]][[2]]
      }
    }
    plot_final <- plots[[1]]
    if (length(plots) >= 2) {
      for (i in 2:length(plots)) {
        plot_final <- plot_final + plots[[i]]
      }
    }

    result$d <- plotResult$new(plot_final, 25, 25, 300)
    result$curr_name <- paste("Test Nr", length(result$names) + 1, "LC50 plot")
    lc50_res$plot_final <- plot_final
  }

  observeEvent(input$lc50, {
    lc50_fct(FALSE)
  })

  output$lc50_result <- renderDT({
    lc50_res$data
  })


  observeEvent(input$lc50Plot, {
    lc50_fct(TRUE)
  })

  output$lc50_plot <- renderPlot({
    lc50_res$plot_final
  })


















  # -------------------------------------------------------------------------------------------------------
  # 9 Examples
  # -------------------------------------------------------------------------------------------------------
  output$allowed_functions <- renderTable({
    df <- data.frame(
      functions = c("-", "+", "*", "/", "log", "log10", "sqrt", "exp", "^", "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan", "is.numeric"),
      functions = c("is.character", "is.logical", "is.factor", "is.integer", "as.numeric", "as.character", "as.logical", "as.factor", "as.integer", ">", "<", "<=", ">=", "==", "!=", "abs", "ceiling", "floor", "trunc"),
      functions = c("round", "grep", "substr", "sub", "paste", "paste0", "strsplit", "tolower", "toupper", "dnorm", "pnorm", "qnorm", "rnorm", "dbinom", "pbinom", "qbinom", "rbinom", "dpois", "ppois"),
      functions = c("rpois", "dunif", "punif", "qunif", "runif", "mean", "sd", "median", "quantile", "range", "sum", "diff", "min", "max", "scale", "c", "vector", "length", "matrix")
    )
  })

  output$example_csv <- renderTable({
    head(CO2, n = 3)
  })

  output$example_cortest <- renderTable({
    broom::tidy(cor.test(CO2$conc, CO2$uptake, data = CO2))
  })

  output$example_levene <- renderTable({
    broom::tidy(car::leveneTest(CO2$uptake ~ CO2$Plant, data = CO2))
  })


  output$example_shapiro <- renderTable({
    correct_name <- function(name, df) {
      name %in% names(df)
    }

    change_char_input <- function(chars) {
      nams <- unlist(strsplit(chars, split = ","))
      for (i in 1:length(nams)) {
        nams[i] <- gsub(" ", "", nams[i])
      }
      nams
    }

    combine <- function(new, vec, df, first) {
      if (length(vec) == 0) {
        return(new)
      }

      if (correct_name(vec[length(vec)], df)) {
        if (isTRUE(first)) {
          new <- df[, vec[length(vec)]]
          first <- FALSE
        } else {
          new <- interaction(new, df[, vec[length(vec)]])
        }
      }

      vec <- vec[-length(vec)]
      combine(new, vec, df, first)
    }

    indep <- "Plant, Type"
    dep <- "uptake"
    dat <- datasets::CO2
    vec <- change_char_input(indep)
    new <- rep("", dim(dat)[1])
    first <- TRUE
    new <- combine(new, vec, dat, first)

    groups <- unique(new)

    check <- TRUE
    res <- NULL

    if (isTRUE(check)) {
      res <- list()
      for (i in groups) {
        posis <- new == i
        temp_dat <- dat[posis, ]

        temp <- NULL
        err <- NULL

        e <- try(
          temp <- broom::tidy(shapiro.test(temp_dat[, dep]))
        )
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
        }
        output$as_error <- renderText(err)

        if (!is.null(temp)) {
          temp$variable <- i
          res[[length(res) + 1]] <- temp
        }
      }

      res <- do.call(rbind, res)
    }

    res
  })


  output$example_ttest <- renderTable({
    broom::tidy(t.test(uptake ~ Type, data = CO2))
  })


  output$example_aov <- renderTable({
    broom::tidy(aov(uptake ~ Treatment * Type, data = CO2))
  })

  output$example_tukey <- renderTable({
    fit <- aov(uptake ~ Plant, data = CO2)
    res <- agricolae::HSD.test(fit,
      trt = "Plant",
      alpha = 0.05, group = TRUE, unbalanced = FALSE
    )$groups

    res[, 1] <- row.names(res)
    res
  })

  output$example_plot <- renderPlot({
    p <- ggplot() +
      geom_boxplot(data = CO2, aes(
        x = Plant, y = uptake, fill = Treatment
      ))
    print(p)
  })
} # end server
