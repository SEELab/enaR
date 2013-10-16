\name{enaStorage}
\alias{enaStorage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Storage analyses of ecological networks.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
enaStorage(x = "network object", balance.override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A network object.  This This includes all weighted flows into and out of
    each vertex as well as the amount of energy--matter stored at each vertex.   
}
  \item{balance.override}{
    %%     ~~Describe \code{balance.override} here~~
    A logical variable.  If FALSE, the functions checks to make sure the
    network model provided is at steady-state.  If TRUE, then the
    function will run without ensuring that the model meets the
    steady-state assumption.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
   \item{C}{output oriented Jacobian community matrix}
  \item{CP}{input oriented Jacobian community matrix}
  \item{S}{dimensionalized integral output community matrix}
  \item{SP}{dimensionalized integral input community matrix}
  \item{Q}{integral output storage matrix - non-dimensional}
  \item{QP}{integral input storage matrix - non-dimensional}
  \item{dt}{selected time step to create P, PP, Q and QP - smallest whole number to make diag(C) nonnegative}
  \item{ns}{vector of the storage based whole system network
    statistics.  These include total system storgae(TSS), storage
    cycling index (CIS), storage output network amplification (NAS),
    storage output network amplification (NASP), input ratio of indirect
    ot direct storage intensities (IDS.i), ouput ratio of indirect to
    direct storage intensities (IDS.o), realized ration of indirect to
    direct storage (IDS.r), output network homogenization (HMG.S.o),
    input network homogenization (HMG.S.i), dominant eigenvalue of P
    (lam1P), ratio of first and second eigenvalues of P (rhoP), dominant
    eigenvalue of PP (lam1PP), ratio of first and second eigenvalues of
    P (rhoPP), and network storage aggradation (AGG.S)}
%% ...
}
\references{

  Matis, J. H., Patten, B. C. 1981. Environ analysis of linear compartmental systems:
  the static, time invariant case.  Bulletin of the International
  Statistical Institute, 48: 527-565.
  
  Fath, B. D., Patten, B. C. 1999.  Review of the foundations of network
  enviorn analysis.  Ecosystems 2:167-179.

  Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network Environ
  Analysis.  Environmental Modelling & Software 21:375-405
  
}

\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)

}
\note{
  %%  ~~further notes~~
  Note that matrices returned are oriented from column to row, which is
  consistant with previous Environ Analysis works.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
%% ~~objects to See Also as \code{\link{help}}, ~~~
\seealso{
    \code{\link{read.scor},\link{read.wand},\link{enaFlow},\link{enaUtility}}
}
\examples{
data(oyster)
S<-enaStorage(oyster)
attributes(S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{enaFlow}
\keyword{read.scor}% __ONLY ONE__ keyword per line
