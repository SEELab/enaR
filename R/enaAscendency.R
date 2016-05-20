#' enaAscendency --- calculates the ascendency statistics
#' of Ulanowicz
#' INPUT = network object
#' OUTPUT = matrix of ascendency statistics
#'
#' D. Hines | December 2011
#' S.R. Borrett | May 2016 - updates
#' ---------------------------------------------------

enaAscendency <- function(x='network object'){

  if (any(is.na(x%v%'export'))|any(is.na(x%v%'respiration'))){
    warning('Model is missing either export or respiration. Calculations may not be correct.')
  }

#'####### set initial conditions for calculations #########
  T.ulan <- as.extended(x)
  N <- ncol(T.ulan) # set up N
  r.td <- c.ld <- t.ulan <- ami <- mat.or.vec(N,N) # initialize ascendency matrix
  oh <- mat.or.vec(N,N) # initialize overhead matrix
  cap <- mat.or.vec(N,N) # initialize capacity matrix
                                        #calculate total system throughPUT
  TSTp <- sum(T.ulan)

#'## calculate H & CAPACITY  #######################################
  #' H = Total Flow Diversity

  h <- T.ulan/sum(T.ulan)
  h2 <- log2(h)
  h2[!is.finite(h2)] <- 0
  H = - sum(h * h2)   # Total Flow Diversity

  CAP <- H * TSTp     # Capactity

#'################### calculate AMI  #######################
  #' AMI = Average Mutual Informaiton

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

#'################ calculate ascendency ###################

  ASC <- TSTp * AMI

#'################ calculate residual diversity  ####################

  Hr <- H - AMI

#'################ calculate overhead  ####################

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


#'################### calculate ratios ####################

                                        # ratio for ascendency/capacity
  ASC.CAP <- ASC/CAP

                                        #ratio for overhead/capacity
  OH.CAP <- OH/CAP

                                        #confirm ratios sum to 1
  ASC.OH.RSUM <- ASC.CAP + OH.CAP

  robustness = -1 * ASC.CAP * log(ASC.CAP)  # robustness from Ulanowicz 2009; Fath 2014



  ################# Calculating Effective Link Density and Trophic Depth ########
  ## Calculate t.ulan 't'

  for (i in 1:N) {
        for (j in 1:N) {
            if (T.ulan[i, j] == 0) {
                t.ulan[i, j] <- 0
            }
            else {
                t.ulan[i, j] <- T.ulan[i,j]/TSTp
            }
        }
    }

    ## Effective Link Density (c)
    for (i in 1:N) {
        for (j in 1:N) {
            if (t.ulan[i, j] == 0) {
                c.ld[i, j] <- 1
            }
            else {
                c.ld[i, j] <- (sqrt(sum(t.ulan[i,])*sum(t.ulan[,j]))/t.ulan[i,j])^(t.ulan[i,j])
            }
        }
    }
    C.LD <- prod(c.ld)

    ## Trophic Depth (r)
    for (i in 1:N) {
        for (j in 1:N) {
            if (t.ulan[i, j] == 0) {
                r.td[i, j] <- 1
            }
            else {
                r.td[i, j] <- (t.ulan[i,j]/(sum(t.ulan[i,])*sum(t.ulan[,j])))^(t.ulan[i,j])
            }
        }
    }
    R.TD <- prod(r.td)

    ELD <- C.LD
    TD <- R.TD
    ##############################################################################


  #'#####################################################################
  # tetrad partition of A, C, O -> input, internal, export, respiration
  #'#####################################################################
  n <- N - 3

  # -- ASCENDENCY --
  # input
  tmp = 0
  for(j in 1:n){
      tmp[j] <-  T.ulan[(n+3),j]  * log2(  (T.ulan[(n+3),j] * TSTp)/( sum(T.ulan[(n+3),]) * sum(T.ulan[,j]) ) )
  }
  A.input <- sum(tmp[!is.nan(tmp)])   # have to remove NaN values

  # internal
  tmp=mat.or.vec(n,n)
  for(i in 1:n){
      for(j in 1:n){
          tmp[i,j]= T.ulan[i,j] * log2(  (T.ulan[i,j] * TSTp) /( sum(T.ulan[i,]) * sum( T.ulan[,j]) ))
      }
  }
  A.internal <- sum(tmp[!is.nan(tmp)])

  # Exports
  tmp <- 0
  for(i in 1:n){
      tmp[i] = T.ulan[i,(n+1)] * log2( (T.ulan[i,(n+1)] * TSTp) / ( sum(T.ulan[,(n+1)]) * sum(T.ulan[i,]) ) )
  }
  A.export <- sum(tmp[!is.nan(tmp)])

  # Respiration
  tmp <- 0
  for(i in 1:n){
      tmp[i] = T.ulan[i,(n+2)] * log2( (T.ulan[i,(n+2)] * TSTp) / ( sum(T.ulan[,(n+2)]) * sum(T.ulan[i,]) ) )
  }
  A.respiration <- sum(tmp[!is.nan(tmp)])

  # -- OVERHEAD --
  # input
  tmp = 0
  for(j in 1:n){
      tmp[j] <-  -1 * T.ulan[(n+3),j]  * log2(  (T.ulan[(n+3),j]^2 )/( sum(T.ulan[(n+3),]) * sum(T.ulan[,j]) ) )
  }
  OH.input <- sum(tmp[!is.nan(tmp)])   # have to remove NaN values

  # internal
  tmp=mat.or.vec(n,n)
  for(i in 1:n){
      for(j in 1:n){
          tmp[i,j]= -1 * T.ulan[i,j] * log2(  (T.ulan[i,j]^2) /( sum(T.ulan[i,]) * sum( T.ulan[,j]) ))
      }
  }
  OH.internal <- sum(tmp[!is.nan(tmp)])

  # export
  tmp <- 0
  for(i in 1:n){
      tmp[i] = -1 * T.ulan[i,(n+1)] * log2( (T.ulan[i,(n+1)]^2) / ( sum(T.ulan[,(n+1)]) * sum(T.ulan[i,]) ) )
  }
  OH.export <- sum(tmp[!is.nan(tmp)])

  # respriation
  tmp <- 0
  for(i in 1:n){
      tmp[i] = -1 *  T.ulan[i,(n+2)] * log2( (T.ulan[i,(n+2)]^2) / ( sum(T.ulan[,(n+2)]) * sum(T.ulan[i,]) ) )
  }
  OH.respiration <- sum(tmp[!is.nan(tmp)])

  # -- CAPACITY --
  CAP.input = A.input + OH.input
  CAP.internal = A.internal + OH.internal
  CAP.export = A.export + OH.export
  CAP.respiration = A.respiration + OH.respiration


  ns <- cbind(H, AMI, Hr, CAP, ASC, OH, ASC.CAP, OH.CAP,
              robustness, ELD, TD,
              A.input, A.internal, A.export, A.respiration,
              OH.input, OH.internal, OH.export, OH.respiration,
              CAP.input, CAP.internal, CAP.export, CAP.respiration)
                                        #
  return(ns)

}
