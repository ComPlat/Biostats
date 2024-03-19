getURL <- function(session) {
  p <- session$clientData
  if (is.null(p)) {
    showNotification("Slot clientData is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  }
  p <- tc(reactiveValuesToList(p), "Could not apply as.list to input")
  p <- p$url_search
  if (is.null(p)) {
    showNotification("Slot url_search is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  } else if (p[[1]] == "") {
    showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- paste0(query$url)
  return(url)
}
