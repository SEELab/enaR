\name{cyberAdj}
\alias{cyberAdj}
\title{
Generate a food-web adjacency matrix
}
\description{
	help function for cyberNet and creates a random trophic network structure (Adjacency matrix) with given number of primary producers, grazers, omnivores, carnivores, detritivores and detrital nodes on the basis of general food-web properties. 
}
\usage{
cyberAdj(M)
}
\arguments{
  \item{M}{
    vector of non-negative integer values. This should give the number of primary producers, grazers, omnivores,carnivores, detrital feeders and detrital nodes in the said order for the desired model. 
  }
  

}

\value{
  Returns a randomly generated adjacency matrix for a digraph that represents the trophic network structure for the desired model to the cyberMod function.
}

\references{
	Fath, B.D. 2004 Network Analysis applied to large scale cyber-ecosystems. Ecological Modelling 171:329--337
}

\author{
Pawandeep Singh (pawan.s@live.co.uk)
}



\seealso{
  \code{\link{cyberNet}}
}
\examples{
M<-rep(2,6)
A <- cyberAdj(M)
# A is a randomly generated trophic ecosystem structure with 2 nodes each of primary producers, grazers, omnivores, carnivores, detritivores and detritus.
}
