\name{ssCheck}
\alias{ssCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Checks the balance of inputs and outputs from a network.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function supports the balancing process by checking if the inputs
and outputs of a given network model are within acceptable limits.
}
\usage{
ssCheck(x = "network object", tol = 5,more=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
The threshold for balance in percent difference between input and
outputs.
}
  \item{more}{
%%     ~~Describe \code{x} here~~
LOGICAL: are any node differences greater than tolerance
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Returns a logical value stating if the model is within acceptable limits
of balance (TRUE) or if it is not (FALSE).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Fath, B.D. and S.R. Borrett. 2006. A MATLAB function for network
environ analysis. Environmental Modelling & Software 21:375-405.
}
\author{
%%  ~~who you are~~
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{balance}}

}
\examples{
data(oyster)
ssCheck(oyster)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
