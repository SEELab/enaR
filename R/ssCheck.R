#' Checks if the given network is out of balance by a given tolerance threshold
#' 
#' This function supports the balancing process by checking if the inputs and outputs of a given network model are within acceptable limits.
#' 
#' @param x A network object.
#' @param tol The threshold for balance in percent difference between input and outputs.
#' @param more LOGICAL: should more detailed results be returned?
#' @param zero.na LOGICAL: should NA values be changed to zeros?
#' @return Returns a logical value stating if the model is within acceptable
#' limits of balance (TRUE) or if it is not (FALSE).
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{balance}}
#' @references Fath, B.D. and S.R. Borrett. 2006. A MATLAB function for network environ analysis. Environmental Modelling & Software 21:375-405.
#' @examples
#' 
#' data(troModels)
#' ssCheck(troModels[[2]])
#' ssCheck(troModels[[6]])
#' 
#' @export ssCheck
ssCheck <- function(x,tol=5,more=FALSE,zero.na=TRUE){
                                        #Check for network class object
  if (class(x) != 'network'){warning('x is not a network class object')}
  T. <- as.extended(x) #convert to extended format
  if (zero.na){T.[is.na(T.)] <- 0}
  n <- network.size(x) #get the number of nodes
  Tin <- apply(T.[,1:n],2,sum) #in throughflow
  Tout <- apply(T.[1:n,],1,sum) #out throughflow
  d <- abs(Tin - Tout) # SSerror difference
  pe <- (d / Tout)*100 # SSerror as percent of total throughflow
                                        #
  if(more==FALSE){
    return(all(pe < tol)) #returns a logical indicating that all node differences are less than tolerance (==TRUE)
  }else{
    return(list("ss"=all(pe < tol),"Tin"=Tin,"Tout"=Tout,"perror"=pe))
  }
}
