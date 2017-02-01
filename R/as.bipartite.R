#' as.bipartite  --- convert a network object to a matrix for 
#' analysis with the bipartite package
#' INPUT = network model
#' OUTPUT = matrix representation
#' M. Lau July 2015
#' ---------------------------------------------------







#' as.bipartite --- convert a network object to a matrix for analysis with the
#' bipartite package INPUT = network model OUTPUT = matrix representation M.
#' Lau July 2015 ---------------------------------------------------
#' as.bipartite --- convert a network object to a matrix for analysis with the
#' bipartite package INPUT = network model OUTPUT = matrix representation M.
#' Lau July 2015 --------------------------------------------------- Create a
#' bipartite network.
#' 
#' Converts a network object (unipartite) to a two-mode (bipartite) network
#' representation.
#' 
#' Bipartite network approaches are often used for analyzing the structure of
#' interactions among species in communities.  Although typically ecosystem
#' networks are handled using a unipartite representation, anlayzing them from
#' a bipartite perspective may be informative. This function provides an easy
#' means for converting to a bipartite representation as long as there is a
#' natural division to categorize species into distinct modes.
#' 
#' @param x A network object.
#' @param y A vector of membership values.
#' @return Returns a matrix with the species of one mode arrayed in rows and
#' the other in columns.
#' @author Matthew K. Lau
#' @examples
#' 
#' 
#' 
#' data(oyster)
#' as.bipartite(oyster, gl(2, 3))
#' 
#' 
#' 
#' @export as.bipartite
as.bipartite <- function(x,y){
    y <- factor(y)
    unpack(x)$F[y == levels(y)[1],y == levels(y)[2]]
}

