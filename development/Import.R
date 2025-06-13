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
    if (a == TRUE) {
      return(as.numeric(b))
    }
    return(b)
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
      names(temp) <- x[1, ]
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
path <- "./development/TestExcel1.xlsx"
read_data_excel(path)

identify_seperator <- function(path) {
  line <- readLines(path, n = 1)
  if(grepl(";", line)) return(";")
  if(grepl(",", line)) return(",")
  if(grepl("\t", line)) return("\t")
  stop("Could not identify the separator. Please upload a file with a known seperator.")
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
path <- "./development/TestCSV1.csv"
read_data_csv(path)
