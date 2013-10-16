# findPathLength --- calculates the flows over a 
# sequence up to a maximum path length
# INPUT = network object
# OUTPUT = a list of flow statistics over paths
# 
# S. Borrett and M. Lau | July 2011
# ---------------------------------------------------

findPathLength <- function(x="network model",maxPath=100,plot.sw=FALSE){

  if(ssCheck(x)=="FALSE"){x = balance(x)}  # ensure the models is balanced

  oo <- get.orient() #original orientation
  if (oo == 'school'){oo <- 'internal'}
  set.orient('internal')
  F <- enaFlow(x)   # perform flow analysis
  set.orient(oo)
                                        #
  TST <- F$ns[2]

                                        # find Total Flow over each path length
  k <- 0:maxPath
  tf <- unlist(lapply(k,function(k) sum( mExp(F$G,k) %*% as.matrix(x%v%'input'))))
  tfi <- tf/TST # total flow intensity flow/TST
  
                                        # find cumulative flow percentage
  k <- 1:(maxPath+1)
  ctf <- unlist(lapply(k, function(k) sum(tfi[1:k])))

                                        # find thresholds
  m50 <- (min(which(ctf>=0.5))-1) # need to subtract 1 becuase index 1 is path length 0.
  m90 <- (min(which(ctf>=0.9))-1)
  m95 <- min(which(ctf>=0.95))-1

  if(F$ns[8]>1){
                                        # find cumulative indirect flow
    direct <- tf[2]   # k =1 is boundary, k = 2 is direct
    k <- 3:(maxPath+1)
    cindirect <- unlist(lapply(k, function(k) sum(tf[3:k]))) 
    mID <- min(which(cindirect>direct))+1
  } else {mID <- NA}
    
  if(plot.sw == TRUE){
    opar <- par(las=1)
    plot(0:(length(ctf)-1),ctf,type="b",pch=20,col="blue",ylim=c(0,1),
         xlab="Path Length",ylab="Cumulative Flow Intensity",axes=FALSE)
    axis(2,at=c(0,0.25,0.5,0.75,0.95,1))
    axis(1,at=c(seq(0,maxPath,by=maxPath/5),m50,m95))
    box()

    points(c(-m50,m50),c(0.5,0.5),type="l",lty=2)
    points(c(m50,m50),c(-m50,0.5),type="l",lty=2)

    points(c(-m95,m95),c(0.95,0.95),type="l",lty=2)
    points(c(m95,m95),c(-m95,0.95),type="l",lty=2)
    rm(opar)
  }
  thresholds <- c("mID"=mID,"m50"=m50,"m90"=m90,"m95"=m95)
 return(list("thresholds"=thresholds,"tf"=tf,"ctf"=ctf))
  
}
