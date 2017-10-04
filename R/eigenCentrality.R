#' the Eigen Centrality of a Network
#' 
#' Calculates the centrality of a network using eigen vectors.
#' 
#' 
#' @param x A matrix defining a network graph.
#' @return Returns the eigen based centrality of the network.
#' @author Stuart R. Borrett Matthew K. Lau
#' @references Bonacich, P., 1987. Power and centrality: a family of measures.
#' American Journal of Sociology 92: 1170-1182.
#' @export eigenCentrality
eigenCentrality <- function(x='matrix'){
  if (class(x) != 'matrix'){warning('x is not a matrix class object')}
                                        # find dominant eigenvector of x
  EVCin <- abs(eigen(x)$vectors[,1])
  EVCin <- EVCin/sum(EVCin)           # normalize by sum
                                        # find dominant eigenvector of x transpose
  EVCout <- abs(eigen(t(x))$vectors[,1])  
  EVCout <- EVCout/sum(EVCout)        # normalize by sum  
  AEVC <- (EVCin + EVCout)/2          # find average eigenvector centrality
  
  return(list('EVCin'=EVCin,'EVCout'=EVCout,'AEVC'=AEVC))
}
