\name{mExp}
\alias{mExp}
\title{
  Calculates the exponent of a matrix.
}
\description{
  Function for calculating the pathway proliferation of flows in a network
  model through matrix exponentiation.
}
\usage{
mExp(x='matrix', n=2)
}
\arguments{
  \item{x}{
    A matrix.
  }
  \item{n}{
    Desired exponent (i.e. the path length).
  }
}

\value{
  Returns an exponentiated flow matrix.
}
\references{
  This function was originally designed by Alberto Monteiro in the
  following R help thread:
  https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html.
}
\author{
  Alberto Monteiro (https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html)
  Matthew K. Lau (mkl48@nau.edu)
}

\seealso{
  \code{\link{findPathLength}}
}

