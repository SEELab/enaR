# enaControl --- control analyses
# INPUT = network object
# OUTPUT = list of control statistics
# 
# M. Lau | July 2011
# ---------------------------------------------------

enaControl <- function(x, zero.na=TRUE,balance.override=FALSE){
                                        #Missing Data Check
  if (any(is.na(x%v%'storage'))){
    warning('This function requires quantified storage values.')
  }else{
                                        #Check for network class
    if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Check for balancing
    if (balance.override == TRUE){}else{
      if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
      if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
    }
                                        #Unpack data
    Ti <- x%v%'input'
    exp <- x%v%'export';exp[is.na(exp)] <- 0
    res <- x%v%'respiration';res[is.na(res)] <- 0
    Tj <- exp + res
    flow <- t(x%n%'flow')
    Q <- enaStorage(x)$Q
    QP <- enaStorage(x)$QP

                                        #Input perspective
    T <- Ti + apply(flow,1,sum) 
    GP <- flow / T
    I <- GP * 0; diag(I) <- 1
    NP <- ginv((I - GP))
    
                                        #Output perspective
    T <- apply(flow,2,sum) + Tj
    G <- t(t(flow) / T)
    I <- G * 0; diag(I) <- 1
    N <- ginv((I - G))

                                        #Calculate the control matrix
    CN <- N / NP
    if (zero.na == TRUE){CN[is.na(CN)] <- 0}
    for (i in (1:nrow(CN))){
      for (j in (1:ncol(CN))){
        if (CN[i,j] < 1){CN[i,j] <- 1 - CN[i,j]}else{CN[i,j] <- 0}
      }
    }

                                        #Storage version
    CQ <- ginv(QP) %*% Q
                                        #Name nodes
    rownames(CN) <- colnames(CN) <- rownames(flow)
    rownames(CQ) <- colnames(CQ) <- rownames(flow)
                                        #Re-orient output
    orient <- get.orient()
    if (orient == 'rc'){
      CN <- t(CN)
      CQ <- t(CQ)
    }else{}
                                        #Package up output
    out <- list('CN'=CN,'CQ'=CQ)

    return(out)
  }
}
