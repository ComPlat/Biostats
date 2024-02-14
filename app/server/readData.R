readData <- function(id, var) {
  moduleServer(
    id,
    function(input, output, session) {
      download_file <- reactive({
        # file <- COMELN::download(session, "https://3.complat-eln-stage.ibcs.kit.edu",  "/home/shiny/results")
        file <- COMELN::download(session, "/home/shiny/results")
        # issue: assumpation is made that ELN is run on the same server!
        # issue; even though file is downloaded it seems that here an error is thrown
        # could it be that here it is tried to download the file several times?
        upload <- function(path) {
          stopifnot(is.character(path))
          df <- NULL
          df <- try( as.data.frame(read_excel(path, col_names = TRUE)), silent = TRUE)
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
        df <- NULL
        df <- upload(file)
        if (is.data.frame(df)) {
          var$df <- df
        } else {
          showNotification("File can not be used. Upload into R failed!", duration = 0)
        }
        tryCatch(
          {
            system(paste("rm -r ", file))
          },
          warning = function(warn) {
            showNotification(paste("A warning occurred: ", conditionMessage(warn)), duration = 0)
          },
          error = function(err) {
            showNotification(paste("An error occurred: ", conditionMessage(err)), duration = 0)
          }
        )
        req(is.data.frame(df))
        return(df)
      })

      observe({
        isolate({
          var$df <- download_file()
        })
      })
    }
  )
}
