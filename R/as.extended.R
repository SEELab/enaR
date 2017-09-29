#' Create an Extended Format Matrix
#' 
#' Converts a network object to the extended format of Allesina and
#' Bondavalli (2003).  Used in the balance function.
#' 
#' @param x A network object.
#' @param zero.na Logical: should NA's be replaced with zeros?
#' @return Returns an extended format matrix.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{balance}}
#' @references Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem
#' flow networks: a comparison between balancing procedures.Ecological
#' Modelling 165(2-3):231-239.
#' @examples
#' 
#' 
#' 
#' data(troModels)
#' as.extended(troModels[[6]])
#' 
#' 
#' 
#' @export as.extended
#' @import network
as.extended <- function(x,zero.na=TRUE){
                                        #Check for network class object
  if (class(x) != "network"){warning('x is not a network class object')}
                                        #unpack the data from the network object
  flow <- as.matrix(x,attrname="flow")
  input <- x%v%'input'
  respiration <- x%v%'respiration'
  export <- x%v%'export'
                                        #recombine into the extended format
  import <- c(input,0,0,0)
  x <- cbind(flow,export,respiration,rep(0,nrow(flow)))
  x <- rbind(x,rep(0,length(import)),rep(0,length(import)),import)
                                        #make NA values zero if zero.na == TRUE
  if (zero.na){
    x[is.na(x)] = 0
  }
  return(x)
}
