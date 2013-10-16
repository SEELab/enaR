\name{balance}
\alias{balance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Balance flow network models.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Applies the methods of Allesina and Bondavalli (2003) for balancing flow
network models.
}
\usage{
balance(x='network object',method=c('AVG2','AVG','IO','OI','I','O'),tol=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Methods for model balancing, see Allesina and Bondavalli (2003).
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
Percent error tolerance used in the steady state check prior to
balancing.
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
Returns a network object with a balanced flow network model.
}
\references{
%% ~put references to the literature/web site here ~
Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem flow
networks: a comparison between balancing procedures. Ecological
Modelling 165(2-3):231-239.
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{bal}}
}
\examples{
###NOT RUN###
# data(oyster)
# balance(oyster)
}
