\name{as.extended}
\alias{as.extended}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create an extended format matrix.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Converts a network object to the extended format of Allesina and
Bondavalli (2003).
}
\usage{
as.extended(x = "network object", zero.na = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{zero.na}{
%%     ~~Describe \code{zero.na} here~~
Logical: should NA's be replaced with zeros.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Used in the balance function. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an extended format matrix.
}
\references{
%% ~put references to the literature/web site here ~
Allesina, S., Bondavalli, C., 2003.Steady state of ecosystem flow
networks: a comparison between balancing procedures.Ecological Modelling 165(2-3):231-239.
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
###NOT RUN###
# data(oyster)
# as.extended(oyster)
}
