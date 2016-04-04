#' enaControl --- control analyses
#' INPUT = network object
#' OUTPUT = list of control statistics
#' M. Lau | July 2011
#' P. Singh | Update Summer 2013
#' S.R. Borrett | Update March 2016

#' ---------------------------------------------------

enaControl <- function(x, zero.na=TRUE,balance.override=FALSE){
                                        #Check for network class
    if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Check for balancing
    if (balance.override){}else{
        if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
        if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
    }

    u <- unpack(x)  # unpack the model
    F <- enaFlow(x, balance.override=balance.override) # perform flow analysis

    # Calculate the control matrix - FLOW
    CN <- F$N / t(F$NP)
    i <- which(CN<1,arr.ind=TRUE)
    j <- which(!(CN<1),arr.ind=TRUE)
    CN[i] <- 1 - CN[i]
    CN[j] <- 0
    if (zero.na){
        CN[!is.finite(CN)] <- 0
    }

    # Calculate the control matrix - STORAGE
    # H:  Storage and Flow provide the same answer
    if(!any(is.na(x%v%'storage'))){
        S <- enaStorage(x,balance.override=balance.override)
        CQ <- S$Q / t(S$QP)
        i <- which(CQ<1,arr.ind=TRUE)
        j <- which(!(CQ<1),arr.ind=TRUE)
        CQ[i] <- 1 - CQ[i]
        CQ[j] <- 0
        if (zero.na){
            CQ[!is.finite(CQ)] <- 0
        }
    } else {
        CQ <- NA
    }

        # Schramski Control Measures (2006, 2007)

    eta <- t(t(F$N) / F$T)  # row-to-column
    CD <- eta - t(eta)      # control difference
    CR <- CD/pmax(eta,t(eta))  # control ratio
    sc <- apply(CD,1,sum)  # system control vector
    psc <- sc/(sum(abs(sc)/2)) * 100 # percent system control vector

    TSC <- sum(abs(sc)/2)
    ns <- c(TSC)


    orient <- get.orient()
    if (orient == 'school'){
        CN <- t(CN)
        CQ <- t(CQ)
        CR <- t(CR)
        CD <- t(CD)
    }

    return(list("CN"=CN,"CQ"=CQ,"CD"=CD,"CR"=CR,"sc"=sc,"psc"=psc, "ns"=ns))
}

