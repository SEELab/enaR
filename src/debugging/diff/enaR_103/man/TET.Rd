\name{TET}
\alias{TET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the total environ throughflow for a ecosystem network model.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Determines the total environ throughflow (TET) for each of the 2 x n environs
of the selected network model. It returns both the TET calculated from a
unit input (output) vector and from the observed or realized input
(output) vector. 
}
\usage{
TET(x = "network object", balance.override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.
}
  \item{balance.override}{
%%     ~~Describe \code{zero.na} here~~
Logical: should the function work if the model is not at steady-state?
}
}

\value{
%%  ~Describe the value returned
  \item{realized.input}{vector of the n realized total
    environ throughflows for the n input oriented environs.}
  \item{realzied.output}{vector of the n realized total
    environ throughflows for the n ouptut oriented environs.}
  \item{unit.input}{vector of the n unit total
    environ throughflows for the n input oriented environs.}
  \item{unit.output}{vector of the n unit total
    environ throughflows for the n output oriented environs.}
}
\references{
%% ~put references to the literature/web site here ~
Gattie, D.K., Schramski, J.R., Borrett, S.R., Patten, B.C., Bata, S.A.,
and Whipple, S.J. 2006. Indirect effects and distributed control in
ecosystems: Network environ analysis of a seven-compartment model of
nitrogen flow in the Neuse River Estuary, USA---Steady-state
analysis. Ecol. Model. 194:162--177.

Whipple, S.J., Borrett, S.R., Patten, B.C., Gattie, D.K., Schramski,
J.R., and Bata, S.A. 2007.  Indirect effects and distributed control in
ecosystems: Comparative network environ analysis of a seven-compartment
model of nitrogen flow in the Neuse River Estuary, USA---Time series
analysis. Ecol. Model. 206: 1--17.
}
\author{
%%  ~~who you are~~
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{enaEnviron}}
}
\examples{
data(oyster)
tet <- TET(oyster)
tet
}
