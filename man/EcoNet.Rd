\name{EcoNet}
\alias{EcoNet}
\title{
  Re-format an EcoNet model.
}
\description{

  This function allows the user to access models that are formatted for
  EcoNet, the web-based interface for conducting ENA
  (http://eco.engr.uga.edu/), by Caner Kazanci at the University of
  Georgia.

}
\usage{
EcoNet(x,verbose=FALSE)
}
\arguments{
  \item{x}{
    An object with the EcoNet formatted file, which can be read into R
    using readLines.
  }
  \item{verbose}{LOGICAL: should warnings be suppressed?}
}
\value{
  Returns the model formatted as a network object.
}
\references{
  Kazanci, C., 2007. EcoNet: A new software for ecological modeling,
  simulation and network analysis, Ecol. Model., Vol 208/1 pp 3-8.
}
\author{
  Matthew K. Lau
}

\seealso{
  \code{\link{EcoNetWeb}}
}


