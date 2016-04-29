\name{signs}
\alias{signs}
\title{
  Signs and summary of input matrix
}
\description{
  Identifies the signs and pairwise relationsips of a given matrix.
  This includes also returns a summary table that provides the
  ecological name of each pairwise realtionship, and a summary of the counts.
}
\usage{
signs(A)
}
\arguments{
  \item{A}{
    A is a square matrix of real numbers.  While this function is more
    general, the intiail intention was for this to be a utility
    matrix or the mixed trophic impacts matrix.
  }

}

\value{
  \item{sign}{a sign matrix for matrix A}
  \item{relations}{a matrix of the pairwise signed relationships in A}
  \item{rs.tab}{table summarizing the pairwise relationships and
    identifying their ecological label}
  \item{relationship.counts}{a count of the different kinds of pairwise
    relationships found in matrix A}
}


\author{
Stuart R. Borrett
}
\note{
   This function is called by relationalChange.
}


\seealso{
  \code{\link{relationalChange}}
}
\examples{
data(oyster)
U <- enaUtility(oyster)$U
s <- signs(U)
}
