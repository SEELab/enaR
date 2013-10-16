\name{findPathLength}
\alias{findPathLength}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Cumulative flow over a range of path lengths.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates the flow throughout the entire network over a given path length.
}
\usage{
findPathLength(x = "network model", maxPath = 100, plot.sw = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Network model object.
}
  \item{maxPath}{
The maximum path length to calculate total flow.
}
  \item{plot.sw}{
LOGICAL: should a plot be generated showing flow accumulation?
}
}

\value{
%%  ~Describe the value returned
 \item{thresholds}{thresholds indicating the development of throughflow
   as path length increases:  the path length at which indirect flow
   exceeds direct flow (mID), path length at which 50\%, 90\%, and 95\%
   of total system throughflow is achieved (m50, m90, and m95, respectively)}
 \item{tf}{total flow across paths from length 0 (Boundary inputs) to maxPath}
 \item{ctf}{cumulative total flow from path length 0 to maxPath}
}
\references{
Borrett, S.R, Patten, B.C., Whipple, S.J. 2010.  Rapid development of
indirect effects in ecological networks.  Oikos 119:1136--1148.  
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as
  \code{\link{enaFlow}}, ~~~
}
\examples{
data(oyster)
pl<-findPathLength(oyster)

pl<-findPathLength(oyster, plot.sw=TRUE,maxPath=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
