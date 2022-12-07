upload <- function(session, filepath) {
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
  
  ret <- upload_intern(token, url, filepath, attachable_id, attachble_type)
  if(ret != 0) {
   showNotification("Error during upload.", duration = 0)
  }
}

upload_intern <- function(token, url, filepath, attachable_id, attachble_type) {
  token_size <- nchar(token)
  url_size <- nchar(url)
  filepath_size <- nchar(filepath)
  attachable_id_size <- nchar(attachable_id)
  attachble_type_size <- nchar(attachble_type)
  .Call("goupload",
        token, url, filepath, attachable_id, attachble_type,
        token_size, url_size, filepath_size, attachable_id_size, attachble_type_size,
        PACKAGE = "COMELN")
}