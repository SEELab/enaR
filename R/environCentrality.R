#' environCentrality --- calculates the centrality of 
#' flow network environs
#' INPUT = environ matrix
#' OUTPUT = in-going, out-going and average centralities
#' 
#' M. Lau | July 2011
#' ---------------------------------------------------





#' environCentrality --- calculates the centrality of flow network environs
#' INPUT = environ matrix OUTPUT = in-going, out-going and average centralities
#' 
#' M. Lau | July 2011 ---------------------------------------------------
#' Environ Centrality an Ecological Network
#' 
#' This function calculates the input, output, and average environ centrality
#' of the nodes in the network (Fath and Borret, 2012).  This is a type of
#' weighted degree centrality that indicates the relative importance of the
#' nodes in the flow activity in the network.
#' 
#' @param x A square matrix.  Usually the integral flow marix from enaFlow. The
#' assumption is that the flows are oriented column to row.
#' @return \item{ECin}{input oriented environ centrality} \item{ECout}{output
#' oriented environ centraility} \item{AEC}{average environ centrality (average
#' of input and output)}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{enaFlow}}
#' @references Fann, S.L. and Borrett, S.R. 2012. Environ centrality reveals
#' the tendency of indirect effects to homogenize the functional importance of
#' species in ecosystems.  Journal of Theoretical Biology 294: 74-86.
#' @examples
#' 
#' 
#' data(troModels)
#' F <- enaFlow(troModels[[6]])
#' ec <- environCentrality(F$N)
#' attributes(ec)
#' barplot(sort(ec$AEC, decreasing = TRUE), col = 4, ylab = "Average Environ Centrality", 
#'     ylim = c(0, 0.4))
#' 
#' 
#' @export environCentrality
environCentrality <- function(x='matrix'){
  if (class(x) != 'matrix'){warning('x is not a matrix class object')}
  ECin <- rowSums(x)/sum(rowSums(x))
  ECout <- colSums(x)/sum(rowSums(x))
  AEC <- (ECin + ECout)/2
  names(ECin) <- rownames(x)
  names(ECout) <- rownames(x)
  names(AEC) <- rownames(x)
  return(list('ECin'=ECin,'ECout'=ECout,'AEC'=AEC))
}
