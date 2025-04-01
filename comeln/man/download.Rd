\name{download}
\alias{download}
\title{download a file from the ELN}
\usage{
  download(
    session,
    folder
  )
}
\arguments{
  \item{session}{A shiny session object}
  \item{folder}{A character string which defines the folder where the downloaded file should be saved}
}
\value{
  A character containing the filename of the downloaded file is returned. 
}
\description{
  The function downloads a file from the ELN. Afterwards, the file is stored within the specified folder. Please make sure that the user has the permission for the folder. 
}
\examples{ 
\dontrun{
  server <- function(input, output, session) {
  
    output$data <- renderDT({
      file <- COMELN::download(session, "/home/shiny/results/")
      file <- paste0("/home/shiny/results/", file)
      df <- read.csv(file, header = TRUE)
      return(df)
    }
  }
}
}