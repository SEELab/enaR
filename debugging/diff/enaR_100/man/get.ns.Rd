\name{get.ns}
\alias{get.ns}
%- Also NEED an '\alias' for EACH other topic documented here.
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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{balance.override}{
Turns off balancing and balance checking.
}
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{st}{Structure statistics}
\item{f}{Flow statistics}
\item{asc}{Ascendency statistics}
\item{s}{Storage statistics}
\item{u.f}{Utility statistics using the flows}
\item{u.s}{Utility statistics using the storage values}

}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.

}
\author{
%%  ~~who you are~~
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
David E. Hines (deh9951@uncw.edu)
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{enaStructure}},\code{\link{enaFlow}},\code{\link{enaAscendency}},\code{\link{enaUtility}}

}
\examples{
data(oyster)
get.ns(oyster)
}
