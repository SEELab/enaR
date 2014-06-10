\name{scc}
\alias{scc}
\title{
  Finds the strongly connected component (SCC) in a graph.
}
\description{
  This function finds the strongly connected components (SCCs) of an
  adjacency matrix A and returns a number of derived network statistics.
}
\usage{
scc(A = "adjacency")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{an n x n adjacency matrix.}
}

\value{
  \item{sp}{a list of structural properties including: the number of
    SCCs ("no.scc"), the number of SCCs with more than 1 node ("no.scc.big"), and the
    fraction of the network nodes participating in a large SCC ("pscc")}
  \item{membership}{numeric vector giving the cluseter id to which each
    node belongs (as in igraph:clusters)}
  \item{scc.id}{numeric vector of the numeric identity in "membership" of SCCs with more than 1 node}
}

\references{
  Allesina, S., Bodini, A., Bondavalli, C., 2005. Ecological subsystems
  via graph theory: the role of strongly connected components. Oikos 110,
  164-176.

  Berman, A., Plemmons, R.J., 1979. Nonnegative Matrices in the
  Mathematical Sciences. Academic Press, New York.

  Borrett, S.R., Fath, B.D., Patten, B.C. 2007. Functional integration of
  ecological networks through pathway proliferation.  Journal of
  Theoretical Biology 245, 98-111. 

}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\note{
  Input matrix is assumed to be oriented from columns to rows.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \link{enaStructure}
}
\examples{
data(troModels)
A<-enaStructure(troModels[[6]])$A
scc(A)
}
