download <- function(session, folder) {
  ipaddress <- getURL(session)
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
  } else if(p[[1]] == "") {
    showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- paste0(ipaddress)
  token <- query$token 
  response <- GET(paste0(
    url,
    "/api/v1/public_third_party_app/download?token=", token
  ))
  if(status_code(response)[[1]] != 200) {
    showNotification("File could not be downloaded from ELN", duration = 0)
    Sys.sleep(30)
  }
  content <- content(response, as = "text")
  tempfile <- tempfile(tmpdir = folder)
  
  fileConn<-file(tempfile)
  writeLines(content, fileConn)
  close(fileConn)
  
  return(tempfile)
}
