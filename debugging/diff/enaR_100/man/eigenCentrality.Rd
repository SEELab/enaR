\name{eigenCentrality}
\alias{eigenCentrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the eigen centrality of a network.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates the eigen centrality of a network.
}
\usage{
eigenCentrality(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A matrix defining a network graph.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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

}
\author{
%%  ~~who you are~~
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
David E. Hines (deh9951@uncw.edu)
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

## The function is currently defined as
function (x) 
{
    if (class(x) != "matrix") {
        warning("x is not a network class object")
    }
    EVCin = abs(eigen(x)$vectors[, 1])
    EVCin = EVCin/sum(EVCin)
    EVCout = abs(eigen(t(x))$vectors[, 1])
    EVCout = EVCout/sum(EVCout)
    AEVC = (EVCin + EVCout)/2
    return(list(EVCin = EVCin, EVCout = EVCout, AEVC = AEVC))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
