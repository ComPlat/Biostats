plotResult <- R6Class("plotResult",
  public = list(
    obj = NULL,
    width = NULL,
    height = NULL,
    dpi = NULL,
    initialize = function(object, width = 10, height = 10, dpi = 300) {
      self$obj <- object
      self$width <- width
      self$height <- height
      self$dpi <- dpi
    }
  )
)

errorClass <- R6Class("errorClass",
  public = list(
    error_message = NULL,
    initialize = function(error_message = NULL) {
      self$error_message <- error_message
    },
    isNull = function() {
      if (is.null(self$error_message)) {
        return(TRUE)
      }
      return(FALSE)
    }
  )
)

