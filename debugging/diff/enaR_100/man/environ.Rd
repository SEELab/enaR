\name{environ}
\alias{environ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the environs for an ecological network.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
environ(x = "network object", input = TRUE, output = TRUE, err.tol = 1e-10, balance.override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{input}{
%%     ~~Describe \code{input} here~~
Should the input environ be calculated? 
}
  \item{output}{
%%     ~~Describe \code{output} here~~
Should the output environ be calculates?
}
  \item{err.tol}{
%%     ~~Describe \code{err.tol} here~~
Error threshold for numerical error fluctuations in flows. Values below err.tol
will be set to zero.
}
  \item{balance.override}{
%%     ~~Describe \code{balance.override} here~~
Logical specifying whether (TRUE) or not (FALSE) the model needs to be
balanced prior to calculations. If TRUE and the model is not balanced,
environs will not be calculated.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The function returns either the input or output environ or both environs
depending upon which were requested.
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
Stuart R. Borrett (borretts@uncw.edu)
Matthew K. Lau (mkl48@nau.edu)
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
