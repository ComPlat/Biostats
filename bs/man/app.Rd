\name{biostats}
\alias{biostats}
\title{Starts the biostats app in your browser}
\usage{
biostats(port)
}
\arguments{
  \item{port}{Optional. A numeric value specifying the port on which to launch the app. Defaults to \code{3838}.}
}
\description{
Launches the Biostats Shiny application in your default web browser.
The app provides an interactive interface for performing and visualizing common statistical analyses.
}
\examples{
\donttest{
bs::biostats()
}
}
