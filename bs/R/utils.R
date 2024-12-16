# Upload data into R
readData <- function(path) {
  stopifnot(is.character(path))
  df <- NULL
  df <- try(as.data.frame(readxl::read_excel(
    path,
    col_names = TRUE
  )), silent = TRUE)
  if (class(df) == "try-error") {
    # identify seperator
    line <- readLines(path, n = 1)
    semicolon <- grepl(";", line)
    comma <- grepl(",", line)
    tab <- grepl("\t", line)
    seperator <- NULL
    if (semicolon == TRUE) {
      seperator <- ";"
    } else if (comma == TRUE) {
      seperator <- ","
    } else if (tab == TRUE) {
      seperator <- "\t"
    } else {
      return("error")
    }
    df <- try(read.csv(path, header = TRUE, sep = seperator))
    if (class(df) == "try-error") {
      return("error")
    }
  } else {
    f <- function(x) {
      options(warn = -1)
      x <- as.numeric(x)
      options(warn = 0)
      x <- x[!is.na(x)]
      length(x) > 0
    }
    check <- apply(df, 2, f)
    conv <- function(a, b) {
      if (a == TRUE) {
        return(as.numeric(b))
      }
      return(b)
    }
    df <- Map(conv, check, df)
    df <- data.frame(df)
  }
  return(df)
}

DF2String <- function(df) {
  resNames <- names(df)
  resNames <- paste(resNames, collapse = "\t")
  resNames <- paste(resNames, "\n")
  res <- apply(df, 1, function(x) {
    x <- as.character(x)
    x <- paste(x, collapse = "\t")
    return(x)
  })
  res <- paste0(resNames, "\n", res, collapse = "")
  res <- paste0(res, "\n")
}

setClass("plot",
  slots = c(
    p = "ANY",
    width = "numeric",
    height = "numeric",
    resolution = "numeric"
  )
)

setClass("diagnosticPlot",
  slots = c(
    p = "character"
  )
)

setClass("doseResponse",
  slots = c(
    df = "data.frame",
    p = "ANY"
  )
)

createExcelFile <- function(l) {
  if (length(l) == 0) {
    print_warn("Nothing to upload")
    return(NULL)
  }

  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")

  curr_row <- 1
  plot_files <- c()
  # save data to excel file
  for (i in seq_along(l)) {
    if (inherits(l[[i]], "plot")) {
      p <- l[[i]]@p
      width <- l[[i]]@width
      height <- l[[i]]@height
      resolution <- l[[i]]@resolution
      fn <- tempfile(fileext = ".png")
      ggsave(
        plot = p,
        filename = fn, width = width, height = height, dpi = resolution
      )
      plot_files <- c(plot_files, fn)
      openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
      curr_row <- curr_row + 20
    } else if (inherits(l[[i]], "diagnosticPlot")) {
      p <- l[[i]]@p
      width <- l[[i]]@width
      height <- l[[i]]@height
      resolution <- l[[i]]@resolution
      fn <- tempfile(fileext = ".png")
      ggsave(
        plot = p,
        filename = fn, width = width, height = height, dpi = resolution
      )
      openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
      curr_row <- curr_row + 20
      plot_files <- c(plot_files, l[[i]]@p)
      plot_files <- c(plot_files, fn)
    } else if (inherits(l[[i]], "doseResponse")) {
      openxlsx::writeData(wb, "Results", l[[i]]@df, startRow = curr_row)
      curr_row <- curr_row + nrow(l[[i]]@df) + 5
      p <- l[[i]]@p
      for (idx in seq_len(length(p))) {
        fn <- tempfile(fileext = ".png")
        ggsave(plot = p[[idx]], filename = fn)
        openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
        curr_row <- curr_row + 20
        plot_files <- c(plot_files, fn)
      }
    } else if (inherits(l[[i]], "data.frame")) {
      openxlsx::writeData(wb, "Results", l[[i]], startRow = curr_row)
      curr_row <- curr_row + dim(l[[i]])[1] + 5
    } else if (is.character(l[[i]])) {
      openxlsx::writeData(wb, "Results", l[[i]], startRow = curr_row)
      curr_row <- curr_row + length(l[[i]])[1] + 5
    }
  }

  # create temporary file
  file <- function() {
    tempfile <- tempfile(tmpdir = "/home/shiny/results", fileext = ".xlsx")
    return(tempfile)
  }
  fn <- file()


  # save workbook
  res <- tryCatch(
    expr = {
      openxlsx::saveWorkbook(wb, fn)
    },
    error = function(e) {
      print_err("Error saving file")
    }
  )

  # Clean up
  for (f in seq_along(plot_files)) {
    unlink(p)
  }

  return(fn)
}

createJSString <- function(l) {
  jsString <- c()
  for (i in seq_along(l)) {
    if (inherits(l[[i]], "plot")) {
      p <- l[[i]]@p
      width <- l[[i]]@width
      height <- l[[i]]@height
      resolution <- l[[i]]@resolution
      fn <- tempfile(fileext = ".png")
      ggsave(
        plot = p,
        filename = fn, width = width, height = height, dpi = resolution
      )
      jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(fn)))
      unlink(fn)
    } else if (inherits(l[[i]], "diagnosticPlot")) {
      jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(l[[i]]@p)))
      unlink(l[[i]]@p)
    } else if (inherits(l[[i]], "doseResponse")) {
      p <- l[[i]]@p
      fn <- tempfile(fileext = ".png")
      for (idx in seq_len(length(p))) {
        fn <- tempfile(fileext = ".png")
        ggsave(plot = p[[idx]], filename = fn)
        jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(fn)))
        unlink(fn)
      }
      unlink(fn)
      jsString <- c(jsString, DF2String(l[[i]]@df))
    } else if (inherits(l[[i]], "data.frame")) {
      jsString <- c(jsString, DF2String(l[[i]]))
    } else if (is.character(l[[i]])) {
      jsString <- c(jsString, l[[i]])
    }
  }
  return(jsString)
}

stackDF <- function(df, keepCol) {
  as.data.frame(pivot_longer(df,
    cols = -keepCol,
    names_to = "name", values_to = "value"
  ))
}

unstackDF <- function(df, name, value) {
  df <- pivot_wider(df, names_from = name, values_from = value)
  df <- map(df, simplify) %>%
    as.data.frame()
  as.data.frame(df)
}

correctName <- function(name, df) {
  name %in% names(df)
}

changeCharInput <- function(chars) {
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
  if (correctName(vec[length(vec)], df)) {
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

splitData <- function(df, formula) {
  df <- model.frame(formula, data = df)
  stopifnot(ncol(df) >= 2)
  res <- data.frame(value = df[, 1], interaction = interaction(df[, 2:ncol(df)]))
  names(res) <- c("value", interaction = paste0(names(df)[2:ncol(df)], collapse = "."))
  res
}

get_elem <- function(df, ...) {
  stopifnot("Expected dataframe or vector" = is.data.frame(df) || is.vector(df))
  s <- substitute(list(...))
  args <- as.list(s[-1])
  l <- length(args)
  if (l <= 0 || l > 2) {
    stop("Wrong number of arguments")
  }
  if (is.data.frame(df) && l == 1) {
    stop("To get one element from a dataframe two index arguments are required")
  }
  if (is.vector(df) && l != 1) {
    stop("To get one element from a list one indec argument is required")
  }
  if (is.data.frame(df)) {
    if (!is.numeric(args[[1]]) || !is.numeric(args[[2]])) {
      stop("The index arguments have to be of type numeric")
    }
    return(df[args[[1]], args[[2]]])
  }
  if (is.vector(df)) {
    if (!is.numeric(args[[1]])) {
      stop("The index arguments have to be of type numeric")
    }
    return(df[args[[1]]])
  }
}

get_cols <- function(df, ...) {
  stopifnot("Expected dataframe" = is.data.frame(df))
  s <- substitute(list(...))
  args <- as.list(s[-1])
  lapply(args, function(x) {
    name <- deparse(x)
    stopifnot("Column not found" = name %in% names(df))
  })
  args <- as.character(args)
  df[, args]
}

get_rows <- function(df, expr) {
  stopifnot("Expected dataframe" = is.data.frame(df))
  subset(df, expr)
}

create_df_name <- function(current_df_name, column_names) {
  if (!(current_df_name %in% column_names)) {
    return(current_df_name)
  }
  counter <- 1
  while (TRUE) {
    current_df_name <- paste0(current_df_name, counter)
    counter <- counter + 1
    if (!(current_df_name %in% column_names)) {
      return(current_df_name)
    }
  }
}

create_r_names <- function(df) {
  names <- sapply(names(df), make.names)
  names(df) <- names
  return(df)
}

as.char <- function(v) {
  return(as.character(v))
}

as.int <- function(v) {
  return(as.integer(v))
}

as.real <- function(v) {
  return(as.numeric(v))
}

as.fact <- function(v) {
  return(as.factor(v))
}

# Split groups
# FIX: this works only for one column
split <- function(df, cols, levels) {
  df_res <- NULL
  levels_temp <- NULL
  for (i in seq_along(cols)) {
    if (i == 1) {
      levels_temp <- levels[levels %in% unique(df[, cols[i]])]
    } else {
      levels_temp <- levels[levels %in% unique(df_res[, cols[i]])]
    }
    df_res <- rbind(df_res, df[df[, cols[i]] == levels_temp, ])
  }
  if (nrow(df) == 0) stop("Subset contains 0 rows")
  return(df_res)
}

# check and print warnings
print_warn <- function(message) {
  showNotification(message, type = "warning")
}

# check and print error
print_err <- function(message) {
  showNotification(message, type = "error")
}

# check and print notifications
print_req <- function(expr, message) {
  if (!expr) {
    showNotification(message, type = "message")
  }
  req(expr)
}

# print notification without check
print_noti <- function(message) {
  showNotification(message, type = "message")
}

# print success
print_success <- function(message) {
  showNotification(message)
}

# check formula and open modal window if no formula is set
print_form <- function(formula) {
  if (is.null(formula)) {
    showNotification("You have to set a formula",
      action = tags$div(
        showModal(modalDialog(
          title = "FormulaEditor",
          FormulaEditorUI("FO"),
          easyClose = TRUE,
          size = "l",
          footer = tagList(
            modalButton("Close")
          )
        ))
      )
    )
  }
  req(!is.null(formula))
}

# Check axis limits
check_axis_limits <- function(col, min, max) {
  if (is.numeric(col)) {
    if (!is.numeric(min) || !is.numeric(max)) {
      stop("Found invalid axis limits")
    }
    if (max <= min) {
      stop("Found invalid axis limits: max <= min")
    }
    return()
  } else {
    choices <- unique(col)
    if (!(min %in% choices) || !(max %in% choices)) {
      stop("Found invalid axis limits")
    }
    if (which(max == choices) <= which(min == choices)) {
      stop("Found invalid axis limits. The max value is found before the min value")
    }
    return()
  }
}

# check that result is only of allowed type
check_type_res <- function(res) {
  allowed <- c("numeric", "integer", "logical", "character", "data.frame")
  if (!(class(res) %in% allowed)) {
    stop(paste0("Found result with unallowed type: ", class(res)))
  }
}

# Check length of input code
check_length_code <- function(code) {
  if (nchar(code) > 4000) {
    stop("The code is too long to be evaluated")
  }
}

# Check that formula is of type response ~ predictor
check_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("Input must be a formula of the type response ~ predictor")
  }
  terms <- all.vars(formula)
  if (length(terms) != 2) {
    stop("Formula must have exactly two terms: response ~ predictor")
  }
  return(TRUE)
}

# Own stats functions handling NA
Mean <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  mean(x, na.rm = TRUE)
}

Median <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  median(x, na.rm = TRUE)
}

SD <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  sd(x, na.rm = TRUE)
}

Sum <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  sum(x, na.rm = TRUE)
}

Min <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  min(x, na.rm = TRUE)
}

Max <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  max(x, na.rm = TRUE)
}

# Check filename
extract_extension <- function(filename) {
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  ex <- ex[[length(ex)]]
  return(ex)
}

is_valid_filename <- function(filename) {
  try({
    if (!is.character(filename)) {
      return(FALSE)
    }
    if (grepl(" ", filename)) {
      return(FALSE)
    }
    invalid_chars <- "[<>:\"/\\|?*]"
    if (grepl(invalid_chars, filename)) {
      return(FALSE)
    }
    if (nchar(filename) == 0) {
      return(FALSE)
    }
    if (nchar(filename) >= 100) {
      return(FALSE)
    }
    ex <- strsplit(basename(filename), split = "\\.")[[1]]
    if (length(ex) == 1) { # no extension found
      return(FALSE)
    }
    return(TRUE)
  })
}

why_filename_invalid <- function(filename) {
  try({
    if (!is.character(filename)) {
      return("Filename has to consist of characters")
    }
    if (grepl(" ", filename)) {
      return("Found spaces in filename")
    }
    invalid_chars <- "[<>:\"/\\|?*]"
    if (grepl(invalid_chars, filename)) {
      return("Found invalid chars in filename: [<>:\"\\|?*")
    }
    if (nchar(filename) == 0) {
      return("Filename is empty")
    }
    if (nchar(filename) >= 100) {
      return("Filename is too long (> 100 characters)")
    }
    ex <- strsplit(basename(filename), split = "\\.")[[1]]
    if (length(ex) == 1) { # no extension found
      return("Filename extension is missing")
    }
    return("")
  })
}

check_filename_for_server <- function(filename) {
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  ex <- ex[[length(ex)]]
  ex == "xlsx"
}

check_filename_for_serverless <- function(filename) {
  ex <- extract_extension(filename)
  ex <- ex[[length(ex)]]
  ex == "zip"
}

# Split list of plots into panels of 9 plots
create_plot_pages <- function(plotList) {
  n_full_pages <- floor(length(plotList) / 9)
  if (n_full_pages == 0) {
    return(list(cowplot::plot_grid(plotlist = plotList)))
  }
  n_plots_last_page <- length(plotList) %% 9
  res <- list()
  i <- 1
  for (i in seq_len(n_full_pages)) {
    if (i == 1) {
      res[[i]] <- plotList[1:(i * 9)]
    } else {
      res[[i]] <- plotList[((i - 1) * 9 + 1):(i * 9)]
    }
  }
  res[[i + 1]] <- plotList[(n_full_pages * 9 + 1):
  (n_full_pages * 9 + n_plots_last_page)]
  lapply(res, function(x) {
    cowplot::plot_grid(plotlist = x)
  })
}

# internal dataframe function
elongate_col <- function(col, l) {
  times <- l / length(col)
  if (floor(times) == times) {
    return(rep(col, times))
  } else {
    res <- rep(col, floor(times))
    remaining_elems <- l %% length(col)
    res <- c(res, col[1:remaining_elems])
    return(res)
  }
}

DataFrame <- function(...) {
  columns <- list(...)
  s <- substitute(list(...))
  args <- as.list(s[-1])
  args <- lapply(args, function(x) {
    make.names(deparse(x))
  })
  sapply(columns, function(x) {
    if (length(x) == 0) stop("Found empty column")
  })
  rows <- max(sapply(columns, length))
  total_bytes <- sum(sapply(columns, function(col) {
    type <- typeof(col)
    element_size <- if (type %in% c("double", "integer", "numeric")) 8 else nchar(type) # Approximate for other types
    rows * element_size
  }))
  if (total_bytes > 10^8) {
    stop("The total size of the data frame is too large")
  }
  columns <- lapply(columns, function(col) {
    elongate_col(col, rows)
  })
  df <- do.call(cbind, columns) |> as.data.frame()
  names(df) <- args
  return(df)
}

Seq <- function(...) {
  args <- list(...)
  start <- args[[1]]
  end <- args[[2]]
  by <- args[[3]]
  number_of_elems <- floor(abs(end - start) / by) + 1
  n_bytes <- number_of_elems * 8 # Assume that each element is a double
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  return(seq(start, end, by))
}

C <- function(...) {
  c(...)
}

Dnorm <- function(...) {
  dnorm(...)
}

Pnorm <- function(...) {
  pnorm(...)
}

Qnorm <- function(...) {
  qnorm(...)
}

Rnorm <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  rnorm(...)
}

Dbinom <- function(...) {
  dbinom(...)
}

Pbinom <- function(...) {
  pbinom(...)
}

Qbinom <- function(...) {
  qbinom(...)
}

Rbinom <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  rbinom(...)
}

Dpois <- function(...) {
  dpois(...)
}

Ppois <- function(...) {
  ppois(...)
}

Rpois <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  rpois(...)
}

Dunif <- function(...) {
  dunif(...)
}

Punif <- function(...) {
  punif(...)
}

Qunif <- function(...) {
  qunif(...)
}

Runif <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  runif(...)
}

