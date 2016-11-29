\name{ShannonDiversity}
\alias{ShannonDiversity}
\title{
  Shannon information entropy
}
\description{
Calculates a number of metrics based on the Shannon information entropy
measure of diversity in a vector, x.
}
\usage{
ShannonDiversity(x = "vector")
}
\arguments{
  \item{x}{a 1 x n vector.}
}

\value{
  \item{H}{Shannon entropy-based metric of diversity.  This captures the
  effects of both richnes (the length of the vector, n) and the
  evenennes of the distribution.}
  \item{Hmax}{The maximum possible value of H given a vector of the
  length n provided.}
  \item{Hr}{Relative evenness Hr = H/Hmax}
  \item{Hcentral}{The centralization or concentration of the values
  among the n elements}
  \item{n}{Number of elements in the vector.}
  \item{effective.n}{effective number of elements in the vector, given
  the distribution of the relative weights.}
}

\author{
  Stuart R. Borrett
}
\note{
}

\seealso{
}
\examples{
data(oyster)

#' throughflow diversity
T <- enaFlow(oyster)$T
ShannonDiversity(T)

#' storage (biomass) biodiversity
X <- oyster%v%'storage'
ShannonDiversity(X)
}
