#' structural statistics
#'
#' This function returns several network statistics that describe a network.
#'
#' @param A An adjacency matrix.
#' @return \item{n}{Number of nodes in A.} \item{L}{Number of direct
#' connections in A.} \item{C}{Connectivity of A.} \item{LD}{Link density.}
#' \item{lam1A}{First dominant eigenvalue of A.} \item{mlam1A}{Multiplicity of the dominant eigenvalue.} \item{lam2A}{Magnitude of the second largest
#' eigenvalue.} \item{rho}{Damping ratio (see Caswell 2001).} \item{R}{Distance of lam1A from the bulk of the eigen spectrum.} \item{d}{Difference between
#' the dominant eigenvalue and the link density.} \item{no.scc}{Number of
#' strongly connected components.} \item{no.scc.big}{Number of strongly
#' connected components greater than 1.} \item{pscc}{Precent of nodes
#' participating in a strongly connected component.}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{enaStructure}},\code{\link{scc}}
#' @references Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network Environ Analysis.  Environ. Model. Softw. 21, 375-405.
structure.statistics <- function(A='adjacency matrix'){
  if (class(A)[[1]] != 'matrix'){warning('A is not a matrix class object')}
                                        #
  n <- dim(A)[1] #number of nodes in A
  L <- sum(A)    #length(A[A!=0]) #number of direct connections in A
  C <- L/n^2 #connectivity of A
  LD <- L/n #link density (equivalent to n*C)
  e <- eigen(A)$values
  aer <- round(abs(e),digits = 7)      # round eigenvalue magnitudes (remove numerical error)
  mlam1A <- length(which(aer == aer[1]))  # find multiplicity of dominant eigenvalue
  ppr <- sum(mExp(A,200))/sum(mExp(A,199)) # pathway proliferation rate (Borrett & Patten 2003)
  lam1A <- abs(e[1])                   # dominant eigenvalue of A. Also termed spectral radius. This is 1) a measure of connectivity, 2) approximately equal to LD and 3) the rate of pathway proliferation
  d <- abs(lam1A-LD)                   # difference between dominant eigenvalue and link density

  if ((n-mlam1A)>0){
      lam2A <- abs(e[(1+mlam1A)]) #magnitude of second largest eigenvalue
      rho <- lam1A/abs(lam2A)  #damping ratio, an indicator of how quickly a^(m)/a^(m-1) foes to lam1[A] (Caswell 2001, p. 95)
      R <- abs(e[n])-abs(e[n-1])/(abs(e[n-1])-abs(e[1])) #distance of lam1[A] from the bulk of the eigen spectrum (Farkas et al. 2001)
  }  else {
      lam2A <- NA;rho <- NA;R <- NA #flag to indicate these do not exist
  }
  sp1 <- as.vector(scc(A)$sp)
  no.scc <- sp1[1]
  no.scc.big <- sp1[2]
  pscc <- sp1[3]
  sp <- cbind(n,L,C,LD,ppr,lam1A,mlam1A,rho,R,d,no.scc,no.scc.big,pscc)  # list of structural statistics of interest
  return(sp)
}
