\name{get.ns}
\alias{get.ns}
\title{
  Quick calculation of a range of network statistics.
}
\description{
  This is a high level function for calculated the main network analyses
  (Ascendancy, Flow, Structure, Storage and Utility) on an ecological
  network. 
}
\usage{
get.ns(x = "network object", balance.override=FALSE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{balance.override}{
    Turns off balancing and balance checking.
  }
}

\value{
  Returns the network statistics (ns) of all of the major ENA functions:
  enaStructure, enaFlow, enaAscendency, enaStorage and enaUtility (both
  flow and storage).
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
David E. Hines (deh9951@uncw.edu)
}

\seealso{
\code{\link{enaStructure}},\code{\link{enaFlow}},\code{\link{enaAscendency}},\code{\link{enaUtility}}

}
\examples{
data(troModels)
get.ns(troModels[[6]])
}
