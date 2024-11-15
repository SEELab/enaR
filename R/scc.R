#' Find the strongly connected component
#'
#' This function finds the strongly connected components (SCCs) of an
#' adjacency matrix A and returns a number of derived network
#' statistics.
#'
#' @param A an n x n adjacency matrix.
#' @return \item{sp}{a list of structural properties including: the number of
#' SCCs ("no.scc"), the number of SCCs with more than 1 node ("no.scc.big"),
#' and the fraction of the network nodes participating in a large SCC ("pscc")}
#' \item{membership}{numeric vector giving the cluseter id to which each node
#' belongs (as in igraph:clusters)} \item{scc.id}{numeric vector of the numeric
#' identity in "membership" of SCCs with more than 1 node}
#' @note Input matrix is assumed to be oriented from columns to rows.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \link{enaStructure}
#' @references Allesina, S., Bodini, A., Bondavalli, C., 2005. Ecological
#' subsystems via graph theory: the role of strongly connected components.
#' Oikos 110, 164-176.
#'
#' Berman, A., Plemmons, R.J., 1979. Nonnegative Matrices in the Mathematical
#' Sciences. Academic Press, New York.
#'
#' Borrett, S.R., Fath, B.D., Patten, B.C. 2007. Functional integration of
#' ecological networks through pathway proliferation.  Journal of Theoretical
#' Biology 245, 98-111.
#' @examples
#'
#'
#'
#' data(troModels)
#' A <- enaStructure(troModels[[6]])$A
#' scc(A)
#'
#'
#' @importFrom sna component.dist
#' @importFrom utils capture.output
#' @export scc
scc <- function(A="adjacency"){
                                        #Check for network class
  if (class(A)[[1]] != 'matrix'){warning('A is not a matrix class object')}
  n <- dim(A)[1]

  log <- capture.output({  # supresses print to screen
      c <- component.dist(A) # finds strong components in A (from sna package)
  })
  no.scc <- length(c$csize)  # numer of scc
  j <- which(c$csize>1)  # finds scc > 1
  no.scc.big <- length(j)    # number of scc > 1
  pscc <- sum(c$csize[j])/n  # percent of nodes participating in a scc
  sp <- c("no.scc"=no.scc,"no.scc.big"=no.scc.big,"pscc"=pscc)
  y <- list("sp"=sp,"membership"=c$membership,"scc.id"=j-1)
  return(y)
}
