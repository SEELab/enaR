\name{read.enam}
\alias{read.enam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Reads in ENA data from an excel file, formatted as Mdloti.xls
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function reads network data from an excel file commonly used by
Ursula Sharler.  The file has three header lines (name/source, number of
compartments, number of living nodes) and then a n+2 x n+2 matrix of
flows.  This is the flow matrix with an additional row for imports and
biomass each and additional columns for exports and respirations.
}
\usage{
read.enam(file='file name and path')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
The name and path for the data file.  This function assumes the data are
stored on the first sheet of an Microsoft Excel formatted. NOTE: this
function depends on the read.xlsx function from the xlsx package, which
requires that the entire path be specified from the root directory
(i.e. the absolute path).
}
}
\value{
Returns the network object.
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.
}
\author{
Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read.scor}}
}

%%%MDMAR02 needs to be added as data

