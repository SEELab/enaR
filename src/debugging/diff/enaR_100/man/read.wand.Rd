\name{read.wand}
\alias{read.wand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Read WAND format files.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Reads WAND formatted network models.
}
\usage{
read.wand(file = "file name with path")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
File path to WAND formatted data file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Returns a network object from a WAND formatted data file. NOTE: this
function depends on the read.xlsx function from the xlsx package, which
requires that the entire path be specified from the root directory
(i.e. the absolute path).
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
Allesina, S., Bondavalli, C., 2004. WAND: an Ecological Network Analysis
user-friendly tool. Environmental Modelling and Software 19(4):337-340.

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
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
