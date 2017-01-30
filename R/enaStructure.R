#' enaStructure --- performes strucutral analysis of the
#' network graph (see Borrett et al. 2007)
#' INPUT = network object
#' OUTPUT = list of structure statistics
#'
#' S. Borrett and M. Lau | March 2011
#' ---------------------------------------------------



#' Structure Analyses of Ecological Network
#' 
#' Analysis of the structure of an ecological flow network.
#' 
#' 
#' @param x A network object.
#' @return \item{A}{
#' 
#' } \item{ns}{A vector of structure based network statistics. These include n
#' = number of nodes, L = number of edges, C = connectivity, LD = link density,
#' ppr = pathway proliferation rate, lam1A = dominant eigenvalue, mlam1A =
#' multiplicity of dominant eigenvalue, rho = damping ratio, R = distance of
#' the dominant eigen value from the eigen spectra, d = difference between
#' dominant eigen value and link density, no.scc = number of strongly connected
#' components, no.scc.big = number of strongly connected components with more
#' than one node, pscc = percent of nodes in strongly connected components.  }
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{structure.statistics}}
#' @references Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
#' Environ Analysis.  Environ. Model. Softw. 21, 375-405.
#' @examples
#' 
#' data(troModels)
#' enaStructure(troModels[[6]])
#' 
#' @export enaStructure
enaStructure <- function(x = 'network object'){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
  Flow <- t(as.matrix(x,attrname = 'flow')) #get flows
  A <- sign(Flow)   # get adjacency matrix
  sp <- structure.statistics(A)    # calls structure.statistics helper function
                                          #Output orientation
  orient <- get.orient()
  if (orient=='rc'){A <- t(A)}else{}
  return(list('A'=A,'ns'=sp))  # "A" is the adjacency matrix oriented
                                        # column to row and "sp" is a list of
                                        # structural network staistics
}
