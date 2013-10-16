\name{TES}
\alias{TES}
\title{
Calculate the total environ storage. 
}
\description{
Calculates the total storage in each n input and output environs.  This
function calculates the storage for both the unit input (output) and the
realized input (output) environs.  Realized uses the observed inputs
(outputs) rather than an assumed unit input (output) to each node.   
}
\usage{
TES(x,balance.override=FALSE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{balance.override}{
    LOGICAL: should balancing being ignored.
  }
}

\value{
  \item{realized.input}{input oriented, realized storage in each environ.}
  \item{realized.output}{output oriented, realized storage in each environ.}
  \item{unit.input }{input oriented, unit storage in each environ.}
  \item{unit.output}{input oriented, unit storage in each environ.}
}
\references{
Matis, J.H. and Patten, B.C. 1981.  Environ analysis of linear
compartmenal systems: the static, time invariant case.  Bulletin of the
International Statistical Institute. 48, 527--565.
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
David E. Hines (deh9951@uncw.edu)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{enaStorage},\link{enaEnviron}}
}
\examples{
data(troModels)
TES(troModels[[6]])
}
