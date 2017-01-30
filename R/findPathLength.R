#' findPathLength --- calculates the flows over a 
#' sequence up to a maximum path length
#' INPUT = network object
#' OUTPUT = a list of flow statistics over paths
#' 
#' S. Borrett and M. Lau | July 2011
#' ---------------------------------------------------



#' Cumulative Flow over a Range of Path Lengths
#' 
#' Calculates the flow throughout the entire network over a given path length.
#' 
#' 
#' @param x Network model object.
#' @param maxPath The maximum path length to calculate total flow.
#' @param plot.sw LOGICAL: should a plot be generated showing flow
#' accumulation?
#' @return \item{thresholds}{thresholds indicating the development of
#' throughflow as path length increases: the path length at which indirect flow
#' exceeds direct flow (mID), path length at which 50\%, 90\%, and 95\% of
#' total system throughflow is achieved (m50, m90, and m95, respectively)}
#' \item{tf}{total flow across paths from length 0 (Boundary inputs) to
#' maxPath} \item{ctf}{cumulative total flow from path length 0 to maxPath}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{enaFlow}}
#' @references Borrett, S.R, Patten, B.C., Whipple, S.J. 2010.  Rapid
#' development of indirect effects in ecological networks.  Oikos
#' 119:1136--1148.
#' @examples
#' 
#' data(troModels)
#' pl10 <- findPathLength(troModels[[6]], plot.sw=TRUE,maxPath=10)
#' names(pl10)
#' pl10$thresholds
#' 
#' @export findPathLength
findPathLength <- function(x,maxPath=100,plot.sw=FALSE){
  ##
  if(ssCheck(x)=="FALSE"){x = balance(x)}  # ensure the models is balanced
  oo <- get.orient() #original orientation
  if (oo == 'school'){oo <- 'internal'}
  set.orient('internal')
  Flow <- enaFlow(x)   # perform flow analysis
  set.orient(oo)
                                        #
  TST <- Flow$ns[2]
                                        # find Total Flow over each path length
  k <- 0:maxPath
  tf <- unlist(lapply(k,function(k) sum( mExp(Flow$G,k) %*% as.matrix(x%v%'input'))))
  tfi <- tf/TST # total flow intensity flow/TST
                                        # find cumulative flow percentage
  k <- 1:(maxPath+1)
  ctf <- unlist(lapply(k, function(k) sum(tfi[1:k])))
                                        # find thresholds
  m50 <- (min(which(ctf>=0.5))-1) # need to subtract 1 becuase index 1 is path length 0.
  m90 <- (min(which(ctf>=0.9))-1)
  m95 <- min(which(ctf>=0.95))-1

  if(Flow$ns[8]>1){
                                        # find cumulative indirect flow
    direct <- tf[2]   # k =1 is boundary, k = 2 is direct
    k <- 3:(maxPath+1)
    cindirect <- unlist(lapply(k, function(k) sum(tf[3:k]))) 
    mID <- min(which(cindirect>direct))+1
  } else {mID <- NA}
    
  if(plot.sw){
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
    par(opar)
    rm(opar)
  }
  thresholds <- c("mID"=mID,"m50"=m50,"m90"=m90,"m95"=m95)
 return(list("thresholds"=thresholds,"tf"=tf,"ctf"=ctf))
  
}
