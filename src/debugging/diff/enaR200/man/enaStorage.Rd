\name{enaStorage}
\alias{enaStorage}
\title{
  Storage analyses of ecological networks.
}
\description{
  Calculates storage-based Ecological Network Analyses.
}
\usage{
enaStorage(x = "network object", balance.override = FALSE)
}
\arguments{
  \item{x}{
    A network object.  This This includes all weighted flows into and out of
    each vertex as well as the amount of energy--matter stored at each vertex.   
  }
  \item{balance.override}{
    LOGICAL: should an imbalanced model be analyzed?  
    If FALSE, the functions checks to make sure the
    network model provided is at steady-state.  If TRUE, then the
    function will run without ensuring that the model meets the
    steady-state assumption.
}
}

\value{
  \item{X}{The storage values themselves.}
  \item{C}{output or donor-storage normalized output-oriented direct flow
  intensity matrix (Jacobian community matrix)}
  \item{S}{dimensionalized integral output community matrix}
  \item{Q}{integral output storage matrix - non-dimensional}
  \item{CP}{input or recipient-storage normalized oriented flow intensity matrix (Jacobian community matrix)}
  \item{SP}{dimensionalized integral input community matrix}
  \item{QP}{integral input storage matrix - non-dimensional}
  \item{dt}{selected time step to create P, PP, Q and QP - smallest whole number to make diag(C) nonnegative}
  \item{ns}{vector of the storage based whole system network statistics.
    These statistics include total system storage (TSS), storage cycling
    index (CIS), Boundary storage intensity (BSI), Direct storage
    intensity (DSI), Indirect storage intensity (ISI), realized ratio of
    indirect-to-direct storage (ID.S), unit input-oriented ratio of
    indirect-to-direct storage intensities (IDS.I), unit output ratio of
    indirect-to-direct storage intensities (IDS.O), input-oriented
    storage-based network homogenization (HMG.S.I), output-oriented
    storage-based network homogenization (HMG.S.O), input-oriented
    storage-based network amplification (AMP.S.I), output-oriented
    storage-based network amplification (AMP.S.O), Storage from Boundary
    flow (mode0.S), storage from internal first passage flow (mode1.S),
    storage from cycled flow (mode2.S), dissipative equivalent to
    mode1.S (mode3.S), dissipative equivalent to mode0.S (mode4.S).}
}

\references{

  Matis, J. H., Patten, B. C. 1981. Environ analysis of linear compartmental systems:
  the static, time invariant case.  Bulletin of the International
  Statistical Institute, 48: 527-565.
  
  Fath, B. D., Patten, B. C. 1999.  Review of the foundations of network
  enviorn analysis.  Ecosystems 2:167-179.

  Fath, B. D. Patten, B. C., Choi, J. 2001.  Compementarity of
  ecological goal functions.   Journal of Theoretical Biology 208: 493-506.

  Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network Environ
  Analysis.  Environmental Modelling & Software 21:375-405
  
}

\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)

}

% \note{
% }

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
