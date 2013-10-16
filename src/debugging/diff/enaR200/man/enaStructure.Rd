\name{enaStructure}
\alias{enaStructure}
\title{
  Structure analyses of ecological network.
}
\description{
  Analysis of the structure of an ecological flow network.
}
\usage{
enaStructure(x = "network object")
}
\arguments{
  \item{x}{
    A network object.
  }
}

\value{
  \item{A}{

  }
  \item{ns}{A vector of structure based network statistics. These
    include n = number of nodes, L = number of edges, C =
    connectivity, LD = link density, ppr = pathway proliferation rate,
    lam1A = dominant eigenvalue, mlam1A = multiplicity of dominant
    eigenvalue, rho = damping ratio,
    R = distance of the dominant eigen value from the eigen spectra, d =
    difference between dominant eigen value and link density, no.scc = number of strongly connected components,
    no.scc.big = number of strongly connected components with more than
    one node, pscc = percent of nodes in strongly connected components.
  }
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
  \code{\link{structure.statistics}}
}
\examples{
data(troModels)
enaStructure(troModels[[6]])
}
