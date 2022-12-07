download <- function(session, folder) {
  p <- session$clientData
  if(is.null(p)) {
    showNotification("Slot clientData is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  }
  p <- tc(reactiveValuesToList(p), "Could not apply as.list to input")
  p <- p$url_search
  if(is.null(p)) {
    showNotification("Slot url_search is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  } else if(p == "") {
    showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- query$url
  url <- paste0("http://", url)
  token <- query$token
  attachable_id <- query$attachable_id
  attachble_type <- query$attachable_type
  session.id <- reactive({ paste0(gsub("-|:| ", "", Sys.time()), as.character(floor(runif(1)*1e20)) ) })
  id <- session.id()  
  ret <- download_intern(token, url, id, folder)
  if(ret != 0) {
   showNotification("Error during download.", duration = 0)
  }

  return(id)
}


download_intern <- function(token, url, filename, path) {
  token_size <- nchar(token)
  url_size <- nchar(url)
  filename_size <- nchar(filename)
  path_size <- nchar(path)
  .Call("godownload",
        token, url, filename, path,
        token_size, url_size, filename_size, path_size,
        PACKAGE = "COMELN")
}

