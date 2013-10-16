\name{force.balance}
\alias{force.balance}
\title{
  Repeated, sequential application the balance function.
}
\description{
  This function repeatedly balances a model, sequentially with the output
  being passed back to the balance function, until it is within tolerance
  or the maximum number of iterations is reached. 
}
\usage{
force.balance(x,tol=5,max.itr=10,method='AVG2')
}

\arguments{
  \item{x}{
    A network object.
  }
  \item{tol}{
    Percent error tolerance for difference between inputs and outputs.
  }
  \item{max.itr}{
    Maximum number iterations.
  }
  \item{method}{
    The balancing method to use, see balance. DEFAULT = AVG2.
  }
}

\value{
  Returns a balanced network model.
}
\references{
  Allesina, S., Bondavalli, C., 2003.Steady state of ecosystem flow
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
ssCheck(troModels[[1]])
fb.model=force.balance(troModels[[2]]) #produces a balanced model
}