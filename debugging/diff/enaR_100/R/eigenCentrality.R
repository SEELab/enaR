# eigenCentrality --- calculates the centrality of a network
# using an eigen vector approach
# INPUT = network matrix
# OUTPUT = list of centrality values (in-going, out-going
# and average)
# S. Borrett | July 2011
# ---------------------------------------------------
eigenCentrality <- function(x){
  if (class(x) != 'matrix'){warning('x is not a matrix class object')}
  EVCin <- abs(eigen(x)$vectors[,1])  # find dominant eigenvector of x
  EVCin <- EVCin/sum(EVCin)           # normalize by sum
  EVCout <- abs(eigen(t(x))$vectors[,1])  # find dominant eigenvector of x transpose
  EVCout <- EVCout/sum(EVCout)        # normalize by sum  
  AEVC <- (EVCin + EVCout)/2          # find average eigenvector centrality
  
  return(list('EVCin'=EVCin,'EVCout'=EVCout,'AEVC'=AEVC))
}
