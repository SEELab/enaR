# ssCheck --- checks if the given network
# is out of balance by a given tolerance
# threshold
# INPUT = network object
# OUTPUT = logical indicating violation of 
# tolerance
# NOTE: used in the balancing process
# M. Lau | July 2011
# ------------------------------------

ssCheck <- function(x,tol=5,more=FALSE,zero.na=TRUE){
                                        #Check for network class object
  if (class(x) != 'network'){warning('x is not a network class object')}
  T <- as.extended(x) #convert to extended format
  if (zero.na){T[is.na(T)] <- 0}
  n <- get.network.attribute(x,'n') #get the number of nodes
  Tin <- sum(T[nrow(T),]) #in throughflow
  Tout <- sum(T[1:n,c(n+1,n+2)]) #out throughflow
  d <- abs(Tin - Tout) # SSerror difference
  pe <- (d / (Tin+Tout))*100 # SSerror as percent of total throughflow
                                        #
  if(more==FALSE){
    return(all(pe < tol)) #returns a logical indicating that all node differences are less than tolerance (==TRUE)
  }else{
    return(list("ss"=all(pe < tol),"Tin"=Tin,"Tout"=Tout,"perror"=pe))
  }  
}
