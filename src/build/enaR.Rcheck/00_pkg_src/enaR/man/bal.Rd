\name{bal}
\alias{bal}
\title{
  Subfunction for balancing by either inputs or outputs.
}
\description{
  Dependency for the \code{balance} function.
}
\usage{
bal(T.star = "extended, unbalanced matrix", method = c("input", "output"))
}
\arguments{
  \item{T.star}{
    Extended, unbalanced matrix.
  }
  \item{method}{
    Balance by inputs or outputs.
  }
}

\value{
  Returns an extended matrix for balancing by inputs or outputs.
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

