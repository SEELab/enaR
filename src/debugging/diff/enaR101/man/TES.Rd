\name{TES}
\alias{TES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate the total environ storage. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates the total storage in each n input and output environs.  This
function calculates the stroage for both the unit input (output) and the
realized input (output) environs.  Realized uses the observed inputs
(outputs) rather than an assumed unit input (output) to each node.   
}
\usage{
TES(x = "network object",balance.override=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{balance.override}{
%%     ~~Describe \code{x} here~~
LOGICAL: should balancing being ignored.
}

}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{realized.input}{input oriented, realized storage in each environ}
  \item{realized.output}{output oriented, realized storage in each environ}
  \item{unit.input }{input oriented, unit storage in each environ}
  \item{unit.output}{input oriented, unit storage in each environ}
}
\references{
%% ~put references to the literature/web site here ~
Matis, J.H. and Patten, B.C. 1981.  Environ analysis of linear
compartmenal systems: the static, time invariant case.  Bulletin of the
International Statistical Institute. 48, 527--565.
}
\author{
%%  ~~who you are~~
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
David E. Hines (deh9951@uncw.edu)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{enaStorage},\link{environ}}
}
\examples{
data(oyster)
tes<-TES(oyster)
}
