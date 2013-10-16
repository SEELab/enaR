\name{pack}
\alias{pack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Compile network information into a network class.
}
\description{
This function provides a flexible framework for importing flow network
information into a network class object for analyses.
}
\usage{
pack(flow,input,export=0,respiration=0,storage=0,living=TRUE,use.output=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow}{
%%     ~~Describe \code{x} here~~
The flow matrix.
}
  \item{input}{
%%     ~~Describe \code{zero.na} here~~
The inputs into the system.
}
  \item{export}{
%%     ~~Describe \code{zero.na} here~~
The exports from the system. If zero, then exports are set to
zero. Use when exports are zero but respiration values are non-zero.
}
  \item{respiration}{
%%     ~~Describe \code{zero.na} here~~
The quantities respired from the system. If zero, then respiration is
set to zero. See export argument above.
}
  \item{storage}{
%%     ~~Describe \code{zero.na} here~~
The quantities stored in compartments within the system. Can be set to
zero.
}
\item{living}{
    %%     ~~Describe \code{zero.na} here~~
    Either a logical vector with length equal to the number of nodes or
    a TRUE = all nodes living or FALSE = all nodes dead.
}
\item{use.output}{
    %%     ~~Describe \code{zero.na} here~~
LOGICAL: if TRUE, respiration and exports are set to NA.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{
Returns a network object for the input model.
}
\references{
%% ~put references to the literature/web site here ~

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
\code{\link{unpack}}
}
\examples{

}
