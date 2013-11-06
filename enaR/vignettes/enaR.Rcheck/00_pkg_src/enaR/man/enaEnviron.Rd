\name{enaEnviron}
\alias{enaEnviron}
\title{
  Ecological network environs.
}
\description{
  Calculates the environs for an ecological network.
}
\usage{
enaEnviron(x,input=TRUE,output=TRUE,type='unit',err.tol=1e-10,balance.override=FALSE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{input}{
    Should the input environ be calculated? 
  }
  \item{output}{
    Should the output environ be calculated?
  }
  \item{type}{
    Specifies the type of environs ("unit" or "realized") to be calculated.
  }
  \item{err.tol}{
    Error threshold for numerical error fluctuations in flows. Values below err.tol
    will be set to zero.
  }
  \item{balance.override}{
    Logical specifying whether (TRUE) or not (FALSE) the model needs to be
    balanced prior to calculations. If TRUE and the model is not balanced,
    environs will not be calculated.
  }

}

\value{
  The function returns the input, output or both environs
  depending upon which were requested.
}
\references{
Fath, B.D. and S.R. Borrett. 2006. A MATLAB function for network
environ analysis. Environmental Modelling & Software 21:375-405.
}
\author{
Stuart R. Borrett (borretts@uncw.edu)
Matthew K. Lau (mkl48@nau.edu)
}

\examples{
data(troModels)
enaEnviron(troModels[[6]])
}
