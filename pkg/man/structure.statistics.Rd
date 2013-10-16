\name{structure.statistics}
\alias{structure.statistics}
\title{
  Calculates the structural statistics (network statistics) of an ecological network.
}
\description{
  This function returns several network statistics that describe a
  network.  
}
\usage{
structure.statistics(A = "adjacency matrix")
}
\arguments{
  \item{A}{An adjacency matrix.}
}

\value{
  \item{n}{Number of nodes in A.}
  \item{L}{Number of direct connections in A.}
  \item{C}{Connectivity of A.}
  \item{LD}{Link density.}
  \item{lam1A}{First dominant eigenvalue of A.}
  \item{mlam1A}{Multiplicity of the dominant eigenvalue.}
  \item{lam2A}{Magnitude of the second largest eigenvalue.}
  \item{rho}{Damping ratio (see Caswell 2001).}
  \item{R}{Distance of lam1A from the bulk of the eigen spectrum.}
  \item{d}{Difference between the dominant eigenvalue and the link density.}
  \item{no.scc}{Number of strongly connected components.}
  \item{no.scc.big}{Number of strongly connected components greater than 1.}
  \item{pscc}{Precent of nodes participating in a strongly connected component.}
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.
}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\seealso{
  \code{\link{enaStructure}},\code{\link{scc}}
}
% \examples{
% }

