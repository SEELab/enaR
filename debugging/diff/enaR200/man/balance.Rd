\name{balance}
\alias{balance}
\title{
Balance flow network models.
}
\description{
  Applies the methods of Allesina and Bondavalli (2003) for balancing flow
  network models.
}
\usage{
balance(x='network object',method=c('AVG2','AVG','IO','OI','I','O'),tol=5)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{method}{
    Methods for model balancing, see Allesina and Bondavalli (2003).
  }
  \item{tol}{
    Percent error tolerance used in the steady state check prior to
    balancing.
  }
}

\value{
  Returns a network object with a balanced flow network model.
}
\references{
Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem flow
networks: a comparison between balancing procedures. Ecological
Modelling 165(2-3):231-239.
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)

}

\seealso{
  \code{\link{bal}}
}
\examples{
data(troModels)
balance(troModels[[6]])
}
