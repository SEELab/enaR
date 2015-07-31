\name{as.bipartite}
\alias{as.bipartite}
\title{
    Create a bipartite network.
}
\description{
    Converts a network object (unipartite) to a 
    two-mode (bipartite) network representation.
}
\usage{
as.bipartite(x, y)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{y}{
      A vector of membership values.
  }

}
\details{
    Bipartite network approaches are often used for analyzing
    the structure of interactions among species in communities.
    Although typically ecosystem networks are handled using a unipartite
    representation, anlayzing them from a bipartite perspective may
    be informative. This function provides an easy means for converting to
    a bipartite representation as long as there is a natural division
    to categorize species into distinct modes.
}
\value{
    Returns a matrix with the species of one mode arrayed
    in rows and the other in columns.
}

\author{
Matthew K. Lau (mkl48@nau.edu)
}

\examples{
data(oyster)
as.bipartite(oyster,gl(2,3))
}
