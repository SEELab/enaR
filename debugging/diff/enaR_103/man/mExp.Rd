\name{mExp}
\alias{mExp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the exponent of a matrix.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function for calculating the pathway proliferation of flows in a network
model through matrix exponentiation.
}
\usage{
mExp(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{mat} here~~
Matrix formatted network model.
}
  \item{n}{
%%     ~~Describe \code{n} here~~
Desired exponent (i.e. the path length).
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an exponentiated flow matrix.
}
\references{
%% ~put references to the literature/web site here ~
This function was originally designed by Alberto Monteiro in the
following R help thread:
https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html.

}
\author{
%%  ~~who you are~~
Alberto Monteiro (https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html)
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{findPathLength}}
}
\examples{
###NOT RUN###
##Isolate the flow matrix from the oyster model.
#x = oyster%n%'flow'
##Calculate the flows over pathways of length three.
#mExp(x,3)
}
