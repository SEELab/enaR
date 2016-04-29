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
relationalChange(x="Direct.U",y="Integral.U")
}
\arguments{
  \item{x}{
    x is a square matrix of real numbers.  While this function is more
    general, the initial intention was for this to be the direct utility
    matrix.
  }
  \item{y}{
    y is a square matrix of real numbers.  While this function is more
    general, the initial intention was for this to be the integral utility
    matrix or the mixed trophic impacts matrix.
  }
}

\value{
  \item{Direct.Signs}{A sign matrix for matrix x.}
  \item{Integral.Signs}{A sign matrix for matrix x.}
  \item{Direct.Relations}{A matrix of the pairwise sign relationships
    for matrix x.}
  \item{Integral.Relations}{A matrix of the pairwise
    signed relationships in matrix y.}
  \item{Relations.Table}{A table that
    summarizes the relations.}
  \item{Changed.Table}{A summary table of only the pariwise relationships
    that changed between x and y.}
  \item{ns}{A vector of network statisitcs which currently
    includes one whole-network statistic - a ratio of the relationships
    changed between x and y.}
}


\author{
  Stuart R. Borrett
}
\note{
  This function is called by enaUtility and enaMTI to summarize results.
}


\seealso{
  \code{\link{enaUtility}, \link{enaMTI}, \link{signs}}
}
\examples{
data(oyster)
D <- enaUtility(oyster)$D
U <- enaUtility(oyster)$U
rc <- relationalChange(D,U)


## To get a count of the number of differnt pairwise relationships in one
## of the sign matrices, you can use the table function

count <- table(rc$Direct.Relations)

}
