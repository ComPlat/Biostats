# TODO: add this in readDF
# add all detected dataframes into the result list
# add a dropdown (maybe next to apply filter) where the user can specify the active table
# add engine class set_active_df
# TODO: allow to define seperator --> reload data and delete file after session end.
# and warn that user shouldn't use comma separated numbers
# Upload data into R
# TODO: check that import works also for server version
is_separator <- function(c) {
  all(is.na(c))
}

scan_cols <- function(df) {
  find_col_start <- function(df, offset) {
    indices <- offset:ncol(df)
    for (i in seq_along(indices)) {
      if (!is_separator(df[, indices[i]])) return(indices[i])
    }
    stop("Found no start column")
  }
  scan_col <- function(df, offset) {
    start <- find_col_start(df, offset)
    end <- start
    indices <- start:ncol(df)
    for (i in seq_along(indices)) {
      if (!is_separator(df[, indices[i]])) {
        end <- end + 1
      } else {
        break
      }
    }
    list(start = start, end = end - 1)
  }
  find_col_indices <- function(df) {
    offset <- 1
    res <- list()
    while(offset <= ncol(df)) {
      res[[length(res) + 1]] <- scan_col(df, offset)
      offset <- res[[length(res)]]$end + 1
    }
    res
  }
  find_col_indices(df)
}

scan_rows <- function(df) {
  find_row_start <- function(df, offset) {
    indices <- offset:nrow(df)
    for (i in seq_along(indices)) {
      if (!is_separator(df[indices[i], ])) return(indices[i])
    }
    stop("Found no start row")
  }
  scan_row <- function(df, offset) {
    start <- find_row_start(df, offset)
    end <- start
    indices <- start:nrow(df)
    for (i in seq_along(indices)) {
      if (!is_separator(df[indices[i], ])) {
        end <- end + 1
      } else {
        break
      }
    }
    list(start = start, end = end - 1)
  }
  find_row_indices <- function(df) {
    offset <- 1
    res <- list()
    while(offset <= nrow(df)) {
      res[[length(res) + 1]] <- scan_row(df, offset)
      offset <- res[[length(res)]]$end + 1
    }
    res
  }
  find_row_indices(df)
}

trim_outer_quotes <- function(v) {
  sapply(v, function(x) {
    if (x == "") return("")
    sub('^"(.*)"$', '\\1', x)
  })
}

cast_types_cols <- function(df) {
  f <- function(x) {
    options(warn = -1)
    x <- as.numeric(x)
    options(warn = 0)
    x <- x[!is.na(x)]
    length(x) > 0
  }
  check <- apply(df, 2, f)
  conv <- function(a, b) {
    if (a) {
      return(as.numeric(b))
    }
    b <- trim_outer_quotes(b)
    return(as.factor(b))
  }
  df <- Map(conv, check, df)
  data.frame(df)
}

extract_tables <- function(df) {
  cols <- scan_cols(df)
  rows <- scan_rows(df)
  stopifnot(
    "Number of columns and rows do not match. Could not properly identify the tables in the file" =
    length(rows) == length(cols)
  )
  tables <- Map(function(rs, cs) {
    df[rs$start:rs$end, cs$start:cs$end]
  }, rows, cols)
  tables[] <- lapply(tables, function(x) {
    if (nrow(x) > 2) {
      temp <- x[2:nrow(x), ]
      names(temp) <- sapply(trim_outer_quotes(x[1, ]), make.names)
      x <- temp
    }
    x
  })
  lapply(tables, cast_types_cols)
}

read_data_excel <- function(path) {
  sheets <- readxl::excel_sheets(path)
  res <- list()
  for (s in sheets) {
    tables <- readxl::read_excel(path, sheet = s, col_names = FALSE) |>
      as.data.frame() |>
      extract_tables()
    res <- c(res, tables)
  }
  res
}

identify_seperator <- function(path) {
  line <- readLines(path, n = 1)
  if(grepl(";", line)) return(";")
  if(grepl(",", line)) return(",")
  if(grepl("\t", line)) return("\t")
  stop("Could not identify the separator. Please upload a file with a known separator.")
}

read_raw <- function(path) {
  sep <- identify_seperator(path)
  raw_content <- readLines(path)
  raw_content <- lapply(raw_content, function(r){
    r <- strsplit(r, split = sep, fixed = TRUE)[[1]]
    r[r == ""] <- NA
    r
  })
  max_cols <- max(lengths(raw_content))
  raw_content <- lapply(raw_content, function(r) {
    length(r) <- max_cols  # pads with NA if too short
    r
  })
  raw_df <- do.call(rbind, raw_content)
  as.data.frame(raw_df, stringsAsFactors = FALSE)
}

read_data_csv <- function(path) {
  read_raw(path) |>
    extract_tables()
}

readData <- function(path, DataModelState, ResultsState) {
  stopifnot(is.character(path))
  if (!file.exists(path)) stop("File does not exists")
  max_file_size <- 50 * 1024^2 # 50 MB in bytes
  file_size <- file.info(path)$size
  if (is.na(file_size) || file_size > max_file_size) {
    stop("File size exceeds the 50 MB limit. Please upload a smaller file.")
  }
  tables <- try(read_data_excel(path), silent = TRUE)
  if (class(tables) == "try-error") {
    tables <- try(read_data_csv(path))
    if (class(tables) == "try-error") {
      stop(conditionMessage(tables))
    }
  }

  DataModelState$df <- tables[[1]]
  if (length(tables) >= 2) {
    lapply(tables, function(t) {
      name <- paste0("df", ResultsState$counter)
      ResultsState$all_data[[name]] <- t
      ResultsState$counter <- ResultsState$counter + 1
    })
  }

  # Check data frame dimensions
  if (nrow(DataModelState$df) == 0) {
    stop("The uploaded file is empty. Please upload a file with data.")
  }
  max_cols <- 1000
  max_rows <- 1e6
  if (nrow(DataModelState$df) > max_rows || ncol(DataModelState$df) > max_cols) {
    stop(sprintf(
      "Data exceeds the limit of %d rows or %d columns. Please upload a smaller dataset.",
      max_rows, max_cols
    ))
  }
}

