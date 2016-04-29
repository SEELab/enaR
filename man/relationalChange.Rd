\name{relationalChange}
\alias{relationalChange}
\title{
  Relational change compared between two matrices.
}
\description{
  Identifies the signs and pairwise relationsips of two matricesa and
  compares the difference between them.
}
\usage{
relational.change(A,B)
}
\arguments{
  \item{A}{
    A is a square matrix of real numbers.  While this function is more
    general, the initial intention was for this to be the direct utility
    matrix.
  }
  \item{B}{
    B is a square matrix of real numbers.  While this function is more
    general, the initial intention was for this to be the integral utility
    matrix or the mixed trophic impacts matrix.
  }
}

\value{
  \item{Direct.Signs}{ a sign matrix for matrix A}
  \item{Integral.Signs}{a sign matrix for matrix A}
  \item{Direct.Relations}{a matrix of the pairwise sign relationships
    for matrix A}
  \item{Integral.Relations}{a matrix of the pairwise
    signed relationships in matrix B}
  \items{Relations.Table}{a table that
    summarizes the relations}
  \item{Changed.Table}{a summary table of only the pariwise relationships
    that changed between matrix A and matrix B}
  \item{ns}{ a vector of network statisitcs which currently
    includes one whole-network statistic - a ratio of the relationships
    changed between matrix A and B.}
}


\author{
Stuart R. Borrett
}
\note{
  This function is called by enaUtility and enaMTI to summarize
  results.
}


\seealso{
  \code{\link{enaUtility}, \link{enaMTI}, \link{signs}}
}
\examples{
data(oyster)
D <- enaUtility(oyster)$D
U <- enaUtility(oyster)$U
rc <- relationalChange(D,U)

# to get a count of the number of differnt pairwise relationships in one of the sign matrices, you can use the table function

count <- table(rc$Direct.Relations)

}
