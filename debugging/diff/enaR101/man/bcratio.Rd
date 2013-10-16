\name{bcratio}
\alias{bcratio}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
Calculates the ratio of positive to negative elements in a network.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Dependent function for the enaUtility function.
}
\usage{
bcratio(x = "matrix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A matrix of flow values.
}
}

\value{
Returns the ratio of positive to negative elements in the flow matrix.
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
Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{enaUtility}}
}
\examples{
###NOT RUN###
#enaUtility(oyster)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
