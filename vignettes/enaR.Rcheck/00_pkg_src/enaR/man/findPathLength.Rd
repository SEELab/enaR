\name{findPathLength}
\alias{findPathLength}
\title{
  Cumulative flow over a range of path lengths.
}
\description{
  Calculates the flow throughout the entire network over a given path length.
}
\usage{
findPathLength(x, maxPath = 100, plot.sw = FALSE)
}
\arguments{
  \item{x}{
    Network model object.
  }
  \item{maxPath}{
    The maximum path length to calculate total flow.
  }
  \item{plot.sw}{
    LOGICAL: should a plot be generated showing flow accumulation?
  }
}

\value{
  \item{thresholds}{thresholds indicating the development of throughflow
    as path length increases:  the path length at which indirect flow
    exceeds direct flow (mID), path length at which 50\%, 90\%, and 95\%
    of total system throughflow is achieved (m50, m90, and m95, respectively)}
  \item{tf}{total flow across paths from length 0 (Boundary inputs) to maxPath}
  \item{ctf}{cumulative total flow from path length 0 to maxPath}
}
\references{
  Borrett, S.R, Patten, B.C., Whipple, S.J. 2010.  Rapid development of
  indirect effects in ecological networks.  Oikos 119:1136--1148.  
}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}


\seealso{
  \code{\link{enaFlow}}
}
\examples{
data(troModels)
pl10 <- findPathLength(troModels[[6]], plot.sw=TRUE,maxPath=10)
names(pl10)
pl10$thresholds
}

