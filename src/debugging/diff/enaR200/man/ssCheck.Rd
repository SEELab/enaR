\name{ssCheck}
\alias{ssCheck}
\title{
  Checks the balance of inputs and outputs from a network.
}
\description{
  This function supports the balancing process by checking if the inputs
  and outputs of a given network model are within acceptable limits.
}
\usage{
ssCheck(x = "network object", tol = 5,more=FALSE,zero.na=TRUE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{tol}{
    The threshold for balance in percent difference between input and
    outputs.
  }
  \item{more}{
    LOGICAL: should more detailed results be returned?
  }
  \item{zero.na}{
    LOGICAL: should NA values be changed to zeros?
  }
}

\value{
  Returns a logical value stating if the model is within acceptable limits
  of balance (TRUE) or if it is not (FALSE).
}

\references{
  Fath, B.D. and S.R. Borrett. 2006. A MATLAB function for network
  environ analysis. Environmental Modelling & Software 21:375-405.
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
ssCheck(troModels[[2]])
ssCheck(troModels[[6]])
}
