#' enaControl --- control analyses
#' INPUT = network object
#' OUTPUT = list of control statistics
#' M. Lau | July 2011
#' P. Singh | Update Summer 2013
#' S.R. Borrett | Update March 2016

#' ---------------------------------------------------


#' enaControl --- control analyses INPUT = network object OUTPUT = list of
#' control statistics M. Lau | July 2011 P. Singh | Update Summer 2013 S.R.
#' Borrett | Update March 2016
#' --------------------------------------------------- enaControl --- control
#' analyses INPUT = network object OUTPUT = list of control statistics M. Lau |
#' July 2011 P. Singh | Update Summer 2013 S.R. Borrett | Update March 2016
#' --------------------------------------------------- Control Analyses of
#' Ecological Networks
#'
#' Analyses for analyzing the control amongst the nodes in ecological networks.
#'
#'
#' @param x A network object.
#' @param zero.na Makes undefined (NA) values zero.
#' @param balance.override Turns off balancing and checks of network balance.
#' @return \item{CN}{Control matrix using flow values.} \item{CQ}{Control
#' matrix using storage values.} \item{CR}{Schramski Control Ratio Matrix}
#' \item{CD}{Schramski Control Difference Matrix} \item{CA}{Control Allocation
#' Matrix} \item{CDep}{Control Dependency Matrix} \item{sc}{Schramski System
#' Control vector} \item{scp}{Schramski system control vector as percent of
#' total control} \item{ns}{vector of network-level summary statistics}
#' @author Matthew K. Lau Stuart R. Borrett Pawandeep Singh
#' @seealso \code{\link{enaStorage}}
#' @references Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network
#' Environ Analysis.  Environmental Modelling & Software 21:375-405
#'
#' Schramski, J.R., Gattie, D.K., Patten, B.C., Borrett S.R., Fath, B.D.,
#' Thomas, C.R., and Whipple, S.J. 2006. Indirect effects and distributed
#' control in ecosystems: Distributed control in the environ networks of a
#' seven compartment model of nitrogen flow in the Neuse River Estuary, USA
#' Steady-state analysis. Ecological Modelling 194:189-201
#'
#' Schramski, J.R., Gattie, D.K., Patten, B.C., Borrett S.R., Fath, B.D., and
#' Whipple, S.J. 2007. Indirect effects and distributed control in ecosystems:
#' Distributed control in the environ networks of a seven compartment model of
#' nitrogen flow in the Neuse River Estuary, USA Time series analysis.
#' Ecological Modelling 206:18-30
#'
#' Chen, S., Fath, B.D., Chen, B. 2011. Information-based network environ
#' analysis: a system perspective for ecologcial risk assessment.  Ecol. Ind.
#' 11:1664-1672.
#'
#' Chen, S. and Chen, B. 2015. Urban energy consumption: Different insights
#' from energy flow analysis, input-output analysis and ecological network
#' analysis.  Applied Energy 138:99-107.
#' @examples
#'
#'
#'
#' data(troModels)
#' enaControl(troModels[[6]])
#'
#'
#'
#' @importFrom MASS ginv
#' @export enaControl
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
    ns <- c("TSC"=TSC)

    # Control Allocation and Control Dependence
    # Chen et al. 2011; Chen and Chen 2015

    d <- F$N - t(F$NP)  # difference
    d[d<0] = 0  # remove negative values
    cd.r <- apply(d,1,sum)
    cd.c <- apply(d,2,sum)

    CA <- ginv(diag(cd.r)) %*% d   # control allocation matrix
    CDep <- ginv(diag(cd.c)) %*% d   # control depedency matrix


    orient <- get.orient()
    if (orient == 'school'){
        CN <- t(CN)
        CQ <- t(CQ)
        CR <- t(CR)
        CD <- t(CD)
        CA <- t(CA)
        CDep <- t(CDep)

    }

    return(list("CN"=CN,"CQ"=CQ,"CD"=CD,"CR"=CR, "CA"=CA, "CDep"=CDep,
                "sc"=sc,"psc"=psc, "ns"=ns))
}

