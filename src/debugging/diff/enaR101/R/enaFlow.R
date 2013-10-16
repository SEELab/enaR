# enaFlow --- flow analysis
# INPUT = network object
# OUTPUT = list of flow statistics
# 
# M. Lau | July 2011
# ---------------------------------------------------
enaFlow <-
function(x='network object',zero.na=TRUE,balance.override=FALSE){

                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override == TRUE){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }

                                        # unpack model
  F <- t(x%n%'flow') #flows
  input <- x%v%'input' #inputs
  stor <- x%v%'storage' #storage values

  n <- nrow(F)      # number of nodes  
  I <- diag(1,nrow(F),ncol(F))          # create identity matrix 
  T <- apply(F,1,sum) + input;   # input throughflow (assuming steady state)

                                        #compute the intercompartmental flows
  GP <- F / T  #Input perspective
  G <- t(t(F) / T)  #Output perspective

                                        #check and replace NA values with 0 if zero.na == TRUE
  if (zero.na == TRUE){
    GP[is.na(GP)] <- 0
    G[is.na(G)] <- 0
    GP[GP == Inf | GP == -Inf] <- 0
    G[G == Inf | G == -Inf] <- 0    
  }

                                        #compute the integral flows
  NP <- ginv((I - GP))
  rownames(NP) <- colnames(NP) <- colnames(GP)
  N <- ginv((I - G))
  rownames(N) <- colnames(N) <- colnames(G)

  ## the ginv function creates noticible numeric error.  I am removing some of it here by rounding
  tol <- 10
  N <- round(N,tol)
  NP <- round(NP,tol)
  

  ## Network Statistics
  TST <- sum(T)  # total system throughflow
  TSTp <- sum(F) + sum(x%v%'input') + sum(x%v%'output') # total system throughput
  
  Boundary <- sum(input)
  APL <- TST/Boundary  # Average Path Lenght (Finn 1976; aka network
                      # aggradation, multiplier effect)
  
                                        # Finn Cycling Index
  p <- as.matrix(rep(1,n),nrow=n)
  dN <- diag(N)
  TSTc <- sum((dN-p)/dN *T)
  FCI <- TSTc/TST
  
                                          # non-locality (realized)
  direct <- sum(G %*% input)
  indirect <- sum((N - I - G) %*% input)
  id <- indirect/direct
  BFI <- Boundary/TST
  DFI <- sum(G %*% input) / TST
  IFI <- indirect/TST
                                        # non-locality (idealized)
  id.O <- sum(N-I-G)/sum(G)
  id.I <- sum(NP-I-GP)/sum(GP)
                                        # HMG
  HMG.O <- ( sd(as.vector(G)) / mean(G) ) / ( sd(as.vector(N)) / mean(N) )
  HMG.I <- ( sd(as.vector(GP)) / mean(GP) ) / ( sd(as.vector(NP)) / mean(NP) )
                                        # Amplification
  AMP.strong.O <- length(which( (N - diag(diag(N))) > 1))
  AMP.strong.I <- length(which( (NP - diag(diag(NP))) > 1))
                                        # MODE ANALYSIS
                                        # This is built from Fath's original MODE program
  mode0 <- Boundary                     # boundary flow
  mode1 <- sum(ginv(diag(diag(N))) %*% N %*% diag(input) - diag(as.vector(I%*%input))) # internal first passage flow
  mode2 <- sum((diag(diag(N))-I) %*% ginv(diag(diag(N))) %*% N %*% diag(input))  # cycled flow
  mode3 <- mode1                      # dissipative equivalent to mode 1
  mode4 <- mode0                      # dissipative equivalent to mode 0

  ns <- cbind(Boundary,TST,TSTp,APL,FCI,BFI,DFI,IFI, id,id.I,id.O,
  HMG.I,HMG.O, AMP.strong.I,AMP.strong.O,
  mode0,mode1,mode2,mode3,mode4)
  
  return(list('T'=T,'G'=G,'GP'=GP,'N'=N,'NP'=NP,'ns'=ns))

}
