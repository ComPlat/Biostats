calcMeans <- function(id, sharedData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$means <- renderTable({
        req(sharedData$data)
        df <- apply(sharedData$data, 2, function(x) {
          x <- as.numeric(x)
          if (is.numeric(x)) {
            return(mean(x))
          }
          return(NA)
        })
        data.frame(t(df))
      })
      
    }
  )
}

