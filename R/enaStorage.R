#' enaStorage --- storage analysis
#' INPUT = network object
#' OUTPUT = list of storage statistics
#'
#' M. Lau | July 2011
#' ---------------------------------------------------




#' enaStorage --- storage analysis INPUT = network object OUTPUT = list of
#' storage statistics
#' 
#' M. Lau | July 2011 ---------------------------------------------------
#' enaStorage --- storage analysis INPUT = network object OUTPUT = list of
#' storage statistics
#' 
#' M. Lau | July 2011 ---------------------------------------------------
#' Storage Analyses of Ecological Networks
#' 
#' Calculates storage-based Ecological Network Analyses.
#' 
#' @param x A network object.  This This includes all weighted flows into and
#' out of each vertex as well as the amount of energy--matter stored at each
#' vertex.
#' @param balance.override LOGICAL: should an imbalanced model be analyzed?  If
#' FALSE, the functions checks to make sure the network model provided is at
#' steady-state.  If TRUE, then the function will run without ensuring that the
#' model meets the steady-state assumption.
#' @return \item{X}{The storage values themselves.} \item{C}{output or
#' donor-storage normalized output-oriented direct flow intensity matrix
#' (Jacobian community matrix)} \item{S}{dimensionalized integral output
#' community matrix} \item{Q}{integral output storage matrix - non-dimensional}
#' \item{CP}{input or recipient-storage normalized oriented flow intensity
#' matrix (Jacobian community matrix)} \item{SP}{dimensionalized integral input
#' community matrix} \item{QP}{integral input storage matrix - non-dimensional}
#' \item{dt}{selected time step to create P, PP, Q and QP - smallest whole
#' number to make diag(C) nonnegative} \item{ns}{vector of the storage based
#' whole system network statistics.  These statistics include total system
#' storage (TSS), storage cycling index (CIS), Boundary storage intensity
#' (BSI), Direct storage intensity (DSI), Indirect storage intensity (ISI),
#' realized ratio of indirect-to-direct storage (ID.S), unit input-oriented
#' ratio of indirect-to-direct storage intensities (IDS.I), unit output ratio
#' of indirect-to-direct storage intensities (IDS.O), input-oriented
#' storage-based network homogenization (HMG.S.I), output-oriented
#' storage-based network homogenization (HMG.S.O), input-oriented storage-based
#' network amplification (AMP.S.I), output-oriented storage-based network
#' amplification (AMP.S.O), Storage from Boundary flow (mode0.S), storage from
#' internal first passage flow (mode1.S), storage from cycled flow (mode2.S),
#' dissipative equivalent to mode1.S (mode3.S), dissipative equivalent to
#' mode0.S (mode4.S).}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso
#' \code{\link{read.scor},\link{read.wand},\link{enaFlow},\link{enaUtility}}
#' @references Matis, J. H., Patten, B. C. 1981. Environ analysis of linear
#' compartmental systems: the static, time invariant case.  Bulletin of the
#' International Statistical Institute, 48: 527-565.
#' 
#' Fath, B. D., Patten, B. C. 1999.  Review of the foundations of network
#' enviorn analysis.  Ecosystems 2:167-179.
#' 
#' Fath, B. D. Patten, B. C., Choi, J. 2001.  Compementarity of ecological goal
#' functions.  Journal of Theoretical Biology 208: 493-506.
#' 
#' Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network Environ
#' Analysis.  Environmental Modelling & Software 21:375-405
#' @keywords enaFlow read.scor
#' @importFrom MASS ginv
#' @export enaStorage
#' @examples
#' data(oyster)
#' S <- enaStorage(oyster)
#' attributes(S)
enaStorage <- function(x,balance.override=FALSE){
                                        #Missing Data Check
  if (any(is.na(x%v%'storage'))){
    warning('This function requires quantified storage values.')
  }else{
                                        #Check for network class
    if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
    if (balance.override){}else{
      if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
      if (x%n%'balanced'){}else{stop('Model is not balanced')}
    }
                                        #unpack data from x
    Flow <- t(as.matrix(x, attrname = 'flow'))  #flows
                                        #continue unpacking
    input <- x%v%'input' #inputs
    stor <- x%v%'storage' #storage values
    T. <- apply(Flow,1,sum) + input
    FD <- Flow - diag(T.) #flow matrix with negative throughflows on the diagonal
    I <- diag(1,nrow(Flow),ncol(Flow)) #create the identity matrix

                                        #Compute the Jacobian matrix
    C <- FD %*% ginv(diag(stor)) #output matrix
    CP <- ginv(diag(stor)) %*% FD #input matrix

                                        #smallest whole number to make diag(C) nonnegative
    dt <- -1 / floor(min(diag(C)))

                                        #calculating the storage-specific, output-oriented, intercompartmental flows (P)
    P <- I + C*dt
    PP <- I + CP*dt
                                        #calculating the dimensionalized integral output and input matrices -- expected residence times (Barber 1979)
    S <- -ginv(C) #output
    SP <- -ginv(CP) #input

    tol <- 10
    S <- round(S,10)
    SP <- round(SP,10)

    # calculate variance of expected residence times (Barber 1979)
    VS <- 2 * ( t(S) %*% diag(diag(S))) - t(S)^2
    VSP <- 2 * (t(SP) %*% diag(diag(SP))) - t(SP)^2


                                        #calculating the integral storage intensity matrix (Q)
    Q <- ginv(I - P) #output
    QP <- ginv(I - PP) #input

  ## the ginv function creates noticible numeric error.  I am removing some of it here by rounding
    tol <- 10
    Q <- round(Q,tol)
    QP <- round(QP,tol)


    dQ <- diag(Q) #diagonal of integral output storage matrix which is the same for input (i.e. diag(QP))

                                        #naming row and columns
    rownames(C) <- colnames(C) <- rownames(Flow)
    rownames(CP) <- colnames(CP) <- rownames(Flow)
    rownames(P) <- colnames(P) <- rownames(Flow)
    rownames(S) <- colnames(S) <- rownames(Flow)
    rownames(VS) <- colnames(VS) <- rownames(Flow)
    rownames(Q) <- colnames(Q) <- rownames(Flow)
    rownames(CP) <- colnames(CP) <- rownames(Flow)
    rownames(PP) <- colnames(PP) <- rownames(Flow)
    rownames(SP) <- colnames(SP) <- rownames(Flow)
    rownames(VSP) <- colnames(VSP) <- rownames(Flow)
    rownames(QP) <- colnames(QP) <- rownames(Flow)

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

    # Amplification parameter
    NAS <- length((Q-diag(diag(Q)))[(Q-diag(diag(Q))) > 1])
    NASP <- length((QP-diag(diag(QP)))[(QP-diag(diag(QP))) > 1])

    # Indirect effects parameter (srb fix 8.3.2011)
    ID.S.O <- sum(Q-I-P) /sum(P)
    ID.S.I <- sum(QP-I-PP) / sum(PP) #indirect to direct ratio (input matrix)

    # Indirect effects parameter (realized)  (srb fix 8.3.2011)
    ID.S <- sum(dt*(as.matrix((Q-I-P)) %*% input)) / sum(dt*as.matrix(P)%*% input ) #indirect to direct ratio (output matrix)

    # Tripartite walk-length division of storage
    BSI = sum( I %*% input *dt) / TSS
    DSI = sum( P %*% input *dt) / TSS
    ISI = sum( (Q-I-P) %*% input *dt) / TSS

    # Homogenization parameter
    CVP <- sd(as.numeric(P)) / mean(P) #Coefficient of variation for G
    CVQ <- sd(as.numeric(Q)) / mean(Q)  #Coefficient of variation for N
    HMG.S.O <- CVP / CVQ #homogenization parameter (output storage)

    CVPP <- sd(as.numeric(PP)) / mean(PP) #Coefficient of variation for GP
    CVQP <- sd(as.numeric(QP)) / mean(QP) #Coefficient of variation for NP
    HMG.S.I <- CVPP / CVQP #homogenization paraemeter (input storage)

    # Network Aggradation
    AGG.S <- TSS / sum(input) #network aggradation -- average amount of storage per system input

    # MODE Partition (Fath et al. 2001)
    z <- unpack(x)$z
    mode0.S = sum(z*dt)  # storage from boundary input flow
    mode1.S = sum( (ginv(diag(diag(Q))) %*% Q - I) %*% diag(z) * dt ) # storage from first-passage flow
    mode2.S = sum( diag(diag(Q) - 1) %*% (ginv(diag(diag(Q))) %*% Q) %*%  diag(z) * dt) # storage from compartment-wise dissipative flow
    mode3.S = mode1.S  # storage from first-passage flow (equal to mode 1 at Steady state)
    mode4.S = sum(unpack(x)$y * dt)  # storage from boundary output flow

    # packing up network statistics for output
    ns <- cbind('TSS'=TSS,
                'CIS'=CIS,
                'BSI'= BSI,'DSI'= DSI,'ISI'= ISI,
                'ID.S'=ID.S, 'ID.S.I'=ID.S.I,'ID.S.O'=ID.S.O,
                'HMG.S.O'=HMG.S.O,'HMG.S.I'=HMG.S.I,
                'NAS'=NAS,'NASP'=NASP,
                mode0.S,mode1.S,mode2.S,mode3.S,mode4.S)

                                        #lam1P'=abs(lam1P),'rhoP'=abs(rhoP),
                                        #lam1PP'=abs(lam1PP),'rhoPP'=abs(rhoPP),'AGG.S'=AGG.S)
                                        #re-orientation
    orient <- get.orient()
    if (orient == 'rc'){
      C <- t(C)
      P <- t(P)
      S <- t(S)
      VS <- t(VS)
      Q <- t(Q)
      CP <- t(CP)
      PP <- t(PP)
      SP <- t(SP)
      VSP <- t(VSP)
      QP <- t(QP)
    }else{}

    out <- list('X'=stor,'C'=C,'P'=P,'S'=S, 'VS'=VS,
                'Q'=Q,'CP'=CP,'PP'=PP,'SP'=SP, 'VSP'=VSP,
                'QP'=QP,'dt'=dt,'ns'=ns)

    return(out)
  }
}
