\name{as.extended}
\alias{as.extended}
\title{
  Create an extended format matrix.
}
\description{
  Converts a network object to the extended format of Allesina and
  Bondavalli (2003).
}
\usage{
as.extended(x, zero.na = TRUE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{zero.na}{
    Logical: should NA's be replaced with zeros?
  }

}
\details{
  Used in the balance function. 
}
\value{
Returns an extended format matrix.
}
\references{
Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem flow
networks: a comparison between balancing procedures.Ecological Modelling 165(2-3):231-239.
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}
\seealso{
  \code{\link{balance}}
}
\examples{
data(troModels)
as.extended(troModels[[6]])
}
