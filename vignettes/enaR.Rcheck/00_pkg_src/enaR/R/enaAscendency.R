# enaAscendency --- calculates the ascendency statistics
# of Ulanowicz
# INPUT = network object
# OUTPUT = matrix of ascendency statistics
# 
# D. Hines | December 2011
# ---------------------------------------------------

enaAscendency <- function(x='network object'){

  if (any(is.na(x%v%'export'))|any(is.na(x%v%'respiration'))){
    warning('Model is missing either export or respiration. Calculations may not be correct.')
  }

######## set initial conditions for calculations #########
  T.ulan <- as.extended(x)
  N <- ncol(T.ulan) # set up N
  ami <- mat.or.vec(N,N) # initialize ascendency matrix
  oh <- mat.or.vec(N,N) # initialize overhead matrix
  cap <- mat.or.vec(N,N) # initialize capacity matrix
                                        #calculate total system throughPUT
  TSTp <- sum(T.ulan)
  
#################### calculate AMI #######################
  
                                        # loop through T.ulan to calculate AMI
  for (i in 1:N){
    for (j in 1:N){
      if (T.ulan[i,j] == 0){
        ami[i,j] <- 0
      }else{
        ami[i,j] <- T.ulan[i,j]/TSTp * log2((T.ulan[i,j]*TSTp)/(sum(T.ulan[,j])*sum(T.ulan[i,])))
      }
    }
  }
  
  AMI <- sum(ami)
  
################# calculate ascendency ###################
  
  ASC <- TSTp*AMI
  
################# calculate overhead  ####################
  
                                        # loop through T.ulan to calculate overhead
  for (i in 1:N){
    for (j in 1:N){
      if (T.ulan[i,j] == 0){
        oh[i,j] <- 0
      }else{
        oh[i,j] <- T.ulan[i,j] * log2((T.ulan[i,j]^2)/(sum(T.ulan[,j])*sum(T.ulan[i,])))
      }
    }
  }
  
  OH <- -sum(oh)
  
############### calculate capacity (long) ###############
  
                                        # loop through T.ulan to calculate capacity
  for (i in 1:N){
    for (j in 1:N){
      if (T.ulan[i,j] == 0){
        cap[i,j] <- 0
      }else{
        cap[i,j] <- T.ulan[i,j] * log2((T.ulan[i,j])/(TSTp))
      }
    }
  }
  
  CAP <- -sum(cap)
  
############## calculate capacity (short) ################
  
  CAP2 <- ASC+OH
  
#################### calculate ratios ####################
  
                                        # ratio for ascendency/capacity
  ASC.CAP <- ASC/CAP
  
                                        #ratio for overhead/capacity
  OH.CAP <- OH/CAP
  
                                        #confirm ratios sum to 1
  ASC.OH.RSUM <- ASC.CAP + OH.CAP
  
  ns <- cbind(AMI,ASC,OH,CAP,ASC.CAP,OH.CAP)
                                        #
  return(ns)
  
}
