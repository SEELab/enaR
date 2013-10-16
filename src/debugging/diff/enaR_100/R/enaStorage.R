# enaStorage --- storage analysis
# INPUT = network object
# OUTPUT = list of storage statistics
# 
# M. Lau | July 2011
# ---------------------------------------------------
enaStorage <- function(x='network object',balance.override=FALSE){

                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override == TRUE){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }

                                        #unpack data from x
  F <- t(x%n%'flow') #flows
  input <- x%v%'input' #inputs
  stor <- x%v%'storage' #storage values
  T <- apply(F,1,sum) + input
  FD <- F - diag(T) #flow matrix with negative throughflows on the diagonal
  I <- diag(1,nrow(F),ncol(F)) #create the identity matrix

                                        #Compute the Jacobian matrix
  C <- FD %*% ginv(diag(stor)) #output matrix
  CP <- ginv(diag(stor)) %*% FD #input matrix
  
                                        #smallest whole number to make diag(C) nonnegative
  dt <- -1 / floor(min(diag(C)))

                                        #calculating the storage-specific, output-oriented, intercompartmental flows (P)
  P <- I + C*dt
  PP <- I + CP*dt
                                        #calculating the dimensionalized integral output and input matrices
  S <- -ginv(C) #output
  SP <- -ginv(CP) #input

                                        #calculating the integral storage intensity matrix (Q)
  Q <- ginv(I - P) #output
  QP <- ginv(I - PP) #input
  dQ <- diag(Q) #diagonal of integral output storage matrix which is the same for input (i.e. diag(QP))

                                        #naming row and columns
  rownames(C) <- colnames(C) <- rownames(F)
  rownames(CP) <- colnames(CP) <- rownames(F)
  rownames(P) <- colnames(P) <- rownames(F)
  rownames(S) <- colnames(S) <- rownames(F)
  rownames(Q) <- colnames(Q) <- rownames(F)
  rownames(CP) <- colnames(CP) <- rownames(F)
  rownames(PP) <- colnames(PP) <- rownames(F)
  rownames(SP) <- colnames(SP) <- rownames(F)
  rownames(QP) <- colnames(QP) <- rownames(F)
  
   ##Storage Environ Properties
                                        #eigen analysis
  e <- eigen(P)$values
  lam1P <- e[1]
  rhoP <- e[1] / e[2]
  
  eP <- eigen(PP)$values
  lam1PP <- eP[1]
  rhoPP <- eP[1] / eP[2]

  TSS <- sum(stor) #total system storage
  TSScs <- sum(((dQ-1)/dQ)%*%stor) #cycled (mode 2) storage
  CIS <- TSScs / TSS #cucling index (storage)

                                        #Amplification parameter
  NAS <- length((Q-diag(diag(Q)))[(Q-diag(diag(Q))) > 1])
  NASP <- length((QP-diag(diag(QP)))[(QP-diag(diag(QP))) > 1])

                                        #Indirect effects parameter (srb fix 8.3.2011)
  IDS.o <- sum(Q-I-P) /sum(P)
  IDS.i <- sum(QP-I-PP) / sum(PP) #indirect to direct ratio (input matrix)

  #Indirect effects parameter (realized)  (srb fix 8.3.2011)
  IDS.r <- sum(dt*(as.matrix((Q-I-P)) %*% as.matrix(x%v%'input'))) / sum(dt*as.matrix(P)%*%as.matrix(x%v%'input')) #indirect to direct ratio (output matrix)

  #Homogenization parameter
  CVP <- sd(as.numeric(P)) / mean(P) #Coefficient of variation for G
  CVQ <- sd(as.numeric(Q)) / mean(Q)  #Coefficient of variation for N
  HMG.S.o <- CVP / CVQ #homogenization parameter (output storage)
  
  CVPP <- sd(as.numeric(P)) / mean(PP) #Coefficient of variation for GP
  CVQP <- sd(as.numeric(QP)) / mean(QP) #Coefficient of variation for NP
  HMG.S.i <- CVPP / CVQP #homogenization paraemeter (input storage)
    
                                        # Network Aggradation
  AGG.S <- TSS / sum(input) #network aggradation -- average amount of storage per system input

  ##packing up network statistics for output
  ns <- cbind('TSS'=TSS,'CIS'=CIS,'NAS'=NAS,'NASP'=NASP,'IDS.i'=IDS.i,'IDS.o'=IDS.o,'IDS.r'=IDS.r,
    'HMG.S.o'=HMG.S.o,'HMG.S.i'=HMG.S.i,
    'lam1P'=abs(lam1P),'rhoP'=abs(rhoP),
    'lam1PP'=abs(lam1PP),'rhoPP'=abs(rhoPP),'AGG.S'=AGG.S)
    
  out <- list('C'=C,'P'=P,'S'=S,'Q'=Q,'CP'=CP,'PP'=P,'SP'=SP,'QP'=Q,'dt'=dt,'ns'=ns)

return(out)

}
