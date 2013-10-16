\name{enaStructure}
\alias{enaStructure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Structure analyses of ecological network.
}
\description{
Analysis of the structure of an ecological flow network.
}
\usage{
enaStructure(x = "network object", balance.override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A network object.
}
  \item{balance.override}{
Turn off balancing and balance checking.
}
}

\value{
 \item{A}{An adjacency matrix oriented column to row.}
 \item{ns}{Structural network statistics.}

}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.

}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{structure.statistics}}
}
\examples{
data(oyster)
enaStructure(oyster)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
