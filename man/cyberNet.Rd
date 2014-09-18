\name{cyberNet}
\alias{cyberNet}
\title{
  cyberNet generates large cyber ecosystems resembling food-webs.
}
\description{
  It returns a network object representing an ecosystem based on the algorithm suggested in Fath, 2004. The user can either specify the number of nodes for each class - primary producers, grazers, omnivores, carnivores, detritivores and detritus, or give an adjacency matrix to distribute flows and create a network model. Inputs can either be supplied or by default the algorithm assigns 100 units of exogenous input to the nodes with no input from other nodes.
}
\usage{
cyberNet(M)
cyberNet(A, inp, non.liv=3)
}
\arguments{
  \item{M}{ vector of non-negative integer values. This should give the number of primary producers, grazers, omnivores,carnivores, detrital feeders and detrital nodes in the said order for the desired model. 
  }
  \item{A}{ adjacency matrix giving the structure of the desired model. In case a specific structure is required instead of a particular number of nodes in each class, the user can input A instead of M vector to assign flows and generate the network object.}
  \item{inp}{ vector of size equal to the number of nodes with exogenous input values for each node. Default is zero which assigns a 100 units of exogenous flow to any node that has no internal input.}
  \item{non.liv}{ non negative integer giving the number of non living nodes in the required network, for the 'living' vertex attribute of the network object. Default is zero, assuming all nodes are living.}

}


\value{
  \item{Y}{Network object depicting the required balanced cyber-ecosystem.}
 
}
\references{
  %% ~put references to the literature/web site here ~
  Fath, B.D. 2004. Network Analysis applied to large scale cyber-ecosystems. Ecological Modelling 171:329-337
}
\author{
  Pawandeep Singh (pawan.s@live.co.uk)
  }
\note{
  The user can either enter the vector M i.e. required number of nodes for different classes or the specific adjacency matrix A, giving the structure wanted for the network object. In either case the function returns a suitable network object with flows, inputs and outputs.
  
}
\seealso{
  \code{\link{cyberAdj}}
}
\examples{
M <- rep(2,6)
net <- cyberNet(M=M, non.liv = 2)
show(net)
# net is a random network object having two nodes each for primary 
# producers, grazers, omnivores, carnivores, detritivores and detritus.
# The exogenious inputs, would be to the two primary producer nodes by 
# the algorithm to resemble a food-web like ecosystem.

A <- c(0,1,0,1,0,0,0,1,0)
A <- matrix(A, nrow=3, byrow=TRUE)
inputs<-c(24,0,3)
mod <- cyberMod(A=A,inp=inputs,non.liv=0)
show(mod)
# mod is the network model with randomly assigned flow values and having the required structure (as A)
}
