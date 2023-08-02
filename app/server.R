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

source("check_ast.R")

server <- function(input, output, session) {

  # -------------------------------------------------------------------------------------------------------
  # 1. download data from ELN and load into shiny
  # -------------------------------------------------------------------------------------------------------
  var <- reactiveValues(df = NULL, token = NULL, url = NULL, id = NULL, type = NULL, ip = NULL, filename_user = NULL)

  output$dat1 <- renderDT({
    
    

    file <- COMELN::download(session, "http://localhost:3000", "/home/shiny/results") 

    # upload file into R
    upload <- function(path) {
      stopifnot(is.character(path))
      df <- NULL
      
      # try read excel file
      df <- try(read_excel(path, col_names = TRUE, col_types = NULL), silent = TRUE )
      
      # if not excel file
      if(class(df) == "try-error") {
        # identify seperator
        line <- readLines(path, n = 1)
        semicolon <- grepl(";", line)
        comma <- grepl(",", line)
        tab <- grepl("\t", line)
        seperator <- NULL
        if(semicolon == TRUE) {
          seperator <- ";"
        } else if(comma == TRUE) {
          seperator <- ","
        } else if(tab == TRUE) {
          seperator <- "\t"
        } else {
          return("error")
        }
        
        df <- try(read.csv(path, header = TRUE, sep = seperator))
        
        if(class(df) == "try-error") {
          return("error")
        }
      } else {
        
        f <- function(x) {
          options(warn=-1)
          x <- as.numeric(x)
          options(warn=0)
          x <- x[!is.na(x)]
          length(x) > 0
        }
        check <- apply(df, 2, f)
        
        conv <- function(a, b) {
          if(a == TRUE) {
            return(as.numeric(b))
          }
          
          return(b)
        }
        df <- Map(conv, check, df)
        df <- data.frame(df)
      }
      
      return(df)
    }
    df <- NULL
    df <- upload(file)

    if(is.data.frame(df)) {
      var$df <- df
    } else {
      showNotification("File can not be used. Upload into R failed!", duration = 0)
    }
    
    if(output != " Error") {
      system(paste("rm -r ", file))
    }
    
    req(is.data.frame(df))

    return(df)
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

    # check ast
    ast <- get_ast(str2lang(op))
    ast <- ast[[length(ast)]]

    if(ast == "Error") {
      showNotification("Unallowed function found")
    } else {
      e <- try(
        new <- with(dat, eval(parse(text = op)))
      )
      if(inherits(e, "try-error")) {
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
      COMELN::upload(session, "http://localhost:3000", checker$file, new_name = var$filename_user)
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
    
    if(length(indices) == 0) {
      showNotification("Nothing to upload")
      return(NULL)
    }

    filename = function(){
      tempfile <- tempfile(tmpdir = "/home/shiny/results", fileext = ".xlsx")
      checker$file <- tempfile
      return(tempfile)
    }
    
    content <- function(file, indices){
      l <- list()

      counter <- 1
      for(i in indices) {
        l[[counter]] <- result$saved[[i]] 
        counter <- counter + 1
      }

      if(length(l) == 0) {
        return(NULL)
      }

      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")

      curr_row <- 1
      for(i in seq_along(1:length(l))) {

        curr_val <- l[[i]]
        if(class(curr_val)[1] != "gg") {
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5
        } else { # find plot element
            tempfile_plot <- tempfile(fileext = ".png")
            png(tempfile_plot)
            print(curr_val)
            dev.off()
            insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
            curr_row <- curr_row + 20
        }

      }

      res <- tryCatch(
        expr = {openxlsx::saveWorkbook(wb, file)},
        error = function(e) {
          "Error"
      })
      return(res)
    }
    
    fn <- filename()
    checker$file <- fn
    content(fn, indices)
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
      fit <- broom::tidy(cor.test(df[,dep1],df[,dep2], method = method, alternative = input$alt0, conf.level = input$conflevel0))
    )
    if(inherits(e, "try-error")) {
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

  output$cor_result <- renderTable({
    res$data
  }, digits = 6)

  # spearman
  observeEvent(input$spear, {
    corr_fct("spearman")
  })

  output$cor_result <- renderTable({
    res$data
  }, digits = 6)

  # kendall
  observeEvent(input$kendall, {
    corr_fct("kendall")
  })

  output$cor_result <- renderTable({
    res$data
  }, digits = 6)



  # -------------------------------------------------------------------------------------------------------
  # 5. plotting
  # -------------------------------------------------------------------------------------------------------

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
  # 5.1 boxplots
  # -------------------------------------------------------------------------------------------------------
  plot_res <- reactiveValues(data = NULL)

  plotfct <- function(method) {
    df <- var$df
    req(df)
    x <- input$x
    y <- input$y
    req(x)
    req(y)

    fill <- input$fill
    col <- input$col

    # check that all names are present in data.frame
    col_names <- names(df)
    check_x <- x %in% col_names
    check_y <- y %in% col_names
    req(check_x)
    req(check_y)

    col <- input$col
    fill <- input$fill
    req( (fill %in% names(df)) || (fill == "") )
    req( (col %in% names(df)) || (col == "") )

    fill_title <- input$legendtitle_col
    col_title <- input$legendtitle_fill

    xlabel <- input$xaxis_text
    ylabel <- input$yaxis_text

    xd <- NULL
    if(input$xtype == "numeric") {
      xd <- as.numeric(df[,x])
    } else {
      xd <- as.factor(df[,x])
    }
    yd <- as.numeric(df[,y])

    if( (fill == "") && (col == "")) {
      p <- ggplot(data = df, aes(x = xd, y = yd)) +
        ylab(ylabel) +
        xlab(xlabel)
    } else if((fill != "") && (col != "") ) {
      p <- ggplot(data = df,
                  aes(x = xd, y = yd,
                  fill = df[,fill], color = df[,col]) ) +
        ylab(ylabel) +
        xlab(xlabel) +
        guides(fill = guide_legend(title = fill_title), col = guide_legend(title = col_title))
    } else if( (fill != "") && (col == "") ) {
      p <- ggplot(data = df,
                  aes(x = xd, y = yd,
                  fill = df[,fill]) ) +
        ylab(ylabel) +
        xlab(xlabel) +
        guides(fill = guide_legend(title = fill_title) )
    } else if( (fill == "") && (col != "") ) {
      p <- ggplot(data = df,
                  aes(x = xd, y = yd,
                  color = df[,col]) ) +
        ylab(ylabel) +
        xlab(xlabel) +
        guides(col = guide_legend(title = col_title) )
    }

    theme <- input$themes
    p <- p +
      scale_color_brewer(palette = theme) +
      scale_fill_brewer(palette = theme)

    if(method == "box") {
      p <- p + geom_boxplot()
    } else if(method == "dot") {
      p <- p + geom_point()
    } else if(method == "line") {
      p <- p + geom_line()
    }

    result$d <- p
    plot_res$data <- renderPlot(p)
    result$curr_name <- paste("Plot Nr", length(result$names) + 1,  paste("Type: ", method))
    output$plot_res <- renderPlot(p)

  }

  observeEvent(input$boxplot, {
    plotfct("box")
  })

  output$plot_res <- renderPlot({
    plot_res$data
  })


  # -------------------------------------------------------------------------------------------------------
  # 5.2 dotplots
  # -------------------------------------------------------------------------------------------------------
  plot_res <- reactiveValues(data = NULL)

  observeEvent(input$dotplot, {
    plotfct("dot")
  })

  output$plot_res <- renderPlot({
    plot_res$data
  })



  # -------------------------------------------------------------------------------------------------------
  # 5.3 Lineplots
  # -------------------------------------------------------------------------------------------------------
  plot_res <- reactiveValues(data = NULL)

  observeEvent(input$lineplot, {
    plotfct("line")
  })

  output$plot_res <- renderPlot({
    plot_res$data
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
      for(i in 1:length(nams)) {
        nams[i]<- gsub(" ", "", nams[i])
      }
      nams
    }

    combine <- function(new, vec, df, first) {

      if(length(vec) == 0) {
        return(new)
      }

      if(correct_name(vec[length(vec)], df)) {

        if(isTRUE(first)) {
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
    if(length(groups) == 1) {
      if(groups == "") {
        output$as_error <- renderText("Something went wrong. Probably not found independent variable(s)")
        check <- FALSE
      }
    }

    if(isTRUE(check)) {
      res <- list()
      for(i in groups) {
        posis <- new == i
        temp_dat <- dat[posis, ]

        temp <- NULL
        err <- NULL

        e <- try(
          temp <- broom::tidy(shapiro.test(temp_dat[, dep]))
        )
        if(inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
        }
        output$as_error <- renderText(err)

        if(!is.null(temp)) {
          temp$variable <- i
          res[[length(res) + 1]] <- temp
        }
      }

      res <- do.call(rbind, res)
    }

    result$d <- res
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "normal distribution (shapiro)")

    output$as_result <- renderTable(res, digits = 6)

  })

  output$as_result <- renderTable({
    as_res$data
  }, digits = 6)


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
    if(inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }


    if(is.null(err)) {
      df <- var$df
      req(df)

      e <- try(
        fit <- broom::tidy(car::leveneTest(formula, data = df, center = input$center))
      )
      if(inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
      }

    }

    as_res$data <- renderTable(fit, digits = 6)
    output$as_result <- renderTable(fit, digits = 6)
    output$as_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "variance homogenity (levene)")
  })

  output$as_result <- renderTable({
    as_res$data
  }, digits = 6)
















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
    if(inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }

    paired <- FALSE
    if(input$paired == "p") {
      paired <- TRUE
    }

    eq <- TRUE
    if(input$vareq == "noeq") {
      eq <- FALSE
    }

    if(is.null(err)) {
      df <- var$df
      req(df)

      e <- try(
        fit <- broom::tidy(t.test(formula, data = df, conf.level = input$conflevel,
                                  alternative = input$alt, paired = paired, var.equal = eq))
      )
      if(inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
      }

    }


    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "t-test or Welch-test")
  })

  output$tests_result <- renderTable({
    tt_res$data
  }, digits = 6)


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
    if(inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    }

    if(is.null(err)) {
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
          if(bal == "Balanced") {
            bal <- TRUE
          } else {
            bal  <- FALSE
          }
          fit <- agricolae::HSD.test(aov_res, trt = indep,
                                     alpha = input$pval, group = TRUE, unbalanced = bal)$groups
        },
        kruskal_test = {
          fit <- with(df, kruskal(df[,dep], df[,indep]),
                      alpha = input$pval, p.adj = input$padj, group = TRUE)$groups
        },
        LSD = {
          aov_res <- aov(formula, data = df)
          fit <- agricolae::LSD.test(aov_res, trt = indep,
                                     alpha = input$pval, p.adj = input$padj, group = TRUE)$groups
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
      if(inherits(e, "try-error")) {
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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "Analysis of variance (AOV)")
  })

  output$tests_result <- renderTable({
    tests_res$data
  } ,digits=6)

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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "Kruskal-Wallis")
  })

  output$tests_result <- renderTable({
    tests_res$data
  }, digits = 6)

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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "TukeyHSD")
  })

  output$tests_result <- renderTable({
    tests_res$data
  }, digits = 6)

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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "Kurskal-Wallis PostHoc test")
  })

  output$tests_result <- renderTable({
    tests_res$data
  }, digits = 6)

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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "LSD")
  })

  output$tests_result <- renderTable({
    tests_res$data
  }, digits = 6)

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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "Scheffe")
  })

  output$tests_result <- renderTable({
    tests_res$data
  }, digits = 6)

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
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }
    tests_res$data <- renderTable(fit, digits = 6)
    output$tests_result <- renderTable(fit, digits = 6)
    output$tests_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "REGW")
  })

  output$tests_result <- renderTable({
    tests_res$data
  }, digits = 6)









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

  lc50_res <- reactiveValues(data = NULL)
  observeEvent(input$lc50, {
    conc <- input$conc
    req(conc)
    names <- input$names
    req(names)
    abs <- input$abs
    req(abs)

    df <- var$df

    fit <- try(MTT::ld50(df[, c(abs, names, conc)]))

    err <- NULL
    if(inherits(fit, "try-error")) {
      err <- conditionMessage(attr(fit, "condition"))
    }

    if(is.null(err)) {
      fit <- cbind(c("LC50", "quality of fit"), fit)
    }

    lc50_res$data <- renderTable(fit, digits = 6)
    output$lc50_result <- renderTable(fit, digits = 6)
    output$lc50_error <- renderText(err)

    result$d <- fit
    result$curr_name <- paste("Test Nr", length(result$names) + 1,  "LC50")
  })

  output$lc50_result <- renderTable({
    lc50_res$data
  }, digits = 6)


















  # -------------------------------------------------------------------------------------------------------
  # 9 Examples
  # -------------------------------------------------------------------------------------------------------
  output$allowed_functions <- renderTable({
    df <- data.frame(
      functions = c("-", "+", "*", "/", "log", "log10", "sqrt", "exp", "^", "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan","is.numeric"),
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
      for(i in 1:length(nams)) {
        nams[i]<- gsub(" ", "", nams[i])
      }
      nams
    }

    combine <- function(new, vec, df, first) {

      if(length(vec) == 0) {
        return(new)
      }

      if(correct_name(vec[length(vec)], df)) {

        if(isTRUE(first)) {
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

    if(isTRUE(check)) {
      res <- list()
      for(i in groups) {
        posis <- new == i
        temp_dat <- dat[posis, ]

        temp <- NULL
        err <- NULL

        e <- try(
          temp <- broom::tidy(shapiro.test(temp_dat[, dep]))
        )
        if(inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
        }
        output$as_error <- renderText(err)

        if(!is.null(temp)) {
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
    broom::tidy(aov(uptake ~ Treatment*Type, data = CO2) )
  })

  output$example_tukey <- renderTable({
    fit <- aov(uptake ~ Plant, data = CO2)
    res <- agricolae::HSD.test(fit , trt = "Plant",
                               alpha = 0.05, group = TRUE, unbalanced = FALSE)$groups

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
