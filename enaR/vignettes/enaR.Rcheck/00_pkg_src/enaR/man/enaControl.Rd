\name{enaControl}
\alias{enaControl}
\title{
  Control analyses of ecological networks.
}
\description{
  Analyses for analyzing the control of ecological networks.
}
\usage{
enaControl(x, zero.na = TRUE, balance.override = FALSE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{zero.na}{
    Makes undefined (NA) values zero.
  }
  \item{balance.override}{
    Turns off balancing and checks of network balance.
  }

}

\value{
\item{CN}{Control matrix using flow values.}
\item{CQ}{Control matrix using storage values.}
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network Environ
  Analysis.  Environmental Modelling & Software 21:375-405
}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}

\seealso{
  \code{\link{enaStorage}}
}
\examples{
data(troModels)
enaControl(troModels[[6]])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
