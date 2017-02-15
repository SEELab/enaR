#' enaFlow --- flow analysis
#' INPUT = network object
#' OUTPUT = list of flow statistics
#'
#' M. Lau | July 2011
#' ---------------------------------------------------







#' enaFlow --- flow analysis INPUT = network object OUTPUT = list of flow
#' statistics
#'
#' M. Lau | July 2011 ---------------------------------------------------
#' enaFlow --- flow analysis INPUT = network object OUTPUT = list of flow
#' statistics
#'
#' M. Lau | July 2011 --------------------------------------------------- Flow
#' Analyses of Ecological Networks
#'
#' Performs the primary throughflow analysis developed for input-output
#' systems.  It returns a vector of throughflows, the input and output oriented
#' matrices for "direct flow intensities" and "integral flow intensities", and
#' a set of flow based network statistics.  Included in the network statistics
#' are a set of measures that describe the diversity of flows in the ecosystem
#' (Ulanowicz's Ascendendy measures).
#'
#' @param x a network object.  This includes all weighted flows into and out of
#' each node.
#' @param zero.na LOGICAL: should NA values be converted to zeros.
#' @param balance.override Flow analysis assumes the network model is at
#' steady-state (inputs = outputs).  Setting balance.override = TRUE allows the
#' function to be run on unbalanced models.
#' @return \item{T}{vector of node throughflows - total amount of
#' energy-matter flowing into or out of each node} \item{G}{matrix of
#' the output oriented direct flow intensities} \item{GP}{matrix of
#' the input oriented direct flow intensities} \item{N}{matrix of the
#' ouput oriented integral (boundary+direct+indirect) flow
#' intensities} \item{NP}{matrix of the input oriented integral flow
#' intensities} \item{TCC}{matrix of total contribution coefficients
#' (Szyrmer & Ulanowicz 1986).  The elements of TCC indicate the
#' fraction of total output of i whihc reaches j} \item{TDC}{matrix of
#' total dependency coefficients (Szyrmer & Ulanowicz 1986).  The
#' elements of TDC indicate the fraction j's total consuption which
#' passes through i} \item{ns}{vector of flow based network
#' statistics.  These include "Boundary" the total input into or
#' output from the system, "TST" the total system throughflow, "TSTp"
#' total system throughPUT,"APL" is the network aggradation
#' TST/Boundary which is also called average path length, "FCI" (Finn
#' Cycling Index) is a metric of the amount of cycling in a system,
#' "BFI" is the boundary flow intensity Boundary/TST, "DFI" is the
#' direct flow intensity Direct/TST, "IFI" is the indirect flow
#' intensity Indirect/TST, "ID.F" is the realized indirect to direct
#' flow intensity, "ID.F.I" is the input idealized indirect flow
#' intensity, "id.F.O"is the output idealized indirect flow intensity,
#' "HMG.I" is the input network homogenization, "HMG.O" is the output
#' network homogenization, "AMP.I" is the strong measure of input
#' network amplifiation, "AMP.O" is the strong measure of output
#' network amplification, "mode0.F" is the boundary flow - flow that
#' reaches a compartment from across the system boundary, "mode1.F" is
#' internal first passage flow, "mode2.F" is cycled flow, "mode3.F" is
#' the dissipative eqivalent to mode2, and "mode4.F" is the
#' dissipative equivalent ot mode0. "H" is the total flow diversity
#' (MacArthur 1955).  Uses the Shannon Information measure (aka
#' Boltzmann entropy) applied to the individual flows, "AMI", is the
#' Average Mutual Information (AMI) in a network. "Hr", is the
#' residual uncertainty that remains about the flow distribution once
#' the ecosystem structure is specified (Hr = H - AMI), "ASC", Returns
#' the ascendnecy of a network, "OH", is the overhead of a network
#' (Ulanowicz 2004), "CAP", is the capacity of a network, "ACS.CAP",
#' is the proportion of capacity used by ascendency, "OH.CAP", Returns
#' the proportion of capacity used by overhead, "robustness", is the
#' robustness of the network, "ELD" Returns the Effective Link Density
#' of the network(c) (Ulanowicz et al. 2014), "TD", Returns the
#' Trophic Depth of the network(r) (Ulanowicz et al. 2014), "A.input",
#' Returns the input ascendnecy of a network, "A.internal", Returns
#' the internal ascendnecy of a network, "A.export", Returns the
#' export ascendnecy of a network, "A.respiration", Returns the
#' respiration ascendnecy of a network, "OH.input", Returns the input
#' overhead of a network, "OH.internal", Returns the internal overhead
#' of a network, "OH.export", Returns the export overhead of a
#' network, "OH.respiration", Returns the respiration overhead of a
#' network, "CAP.input", Returns the input capacity of a network,
#' "CAP.internal", Returns the internal capacity of a network,
#' "CAP.export", Returns the export capacity of a network,
#' "CAP.respiration", Returns the respiration capacity of a network}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso
#' \code{\link{read.scor},\link{read.wand},\link{enaStorage},\link{enaUtility}}
#' @references Borrett, S. R., Freeze, M. A., 2011. Reconnecting environs to
#' their environment. Ecol. Model. 222, 2393-2403.
#'
#' Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network Environ
#' Analysis.  Environ. Model. Softw. 21, 375-405.
#'
#' Fath, B. D., Patten, B. C., 1999. Review of the foundations of network
#' environ analysis. Ecosystems 2, 167-179.
#'
#' Finn, J. T., 1976. Measures of ecosystem structure and function derived from
#' analysis of flows. J. Theor. Biol. 56, 363-380.
#'
#' Patten, B.C. Higashi, M., Burns, T. P. 1990. Trophic dynamics in ecosystem
#' networks: significance of cycles and storage.  Ecol. Model. 51, 1-28.
#'
#' Schramski, J. R., Kazanci, C., Tollner, E. W., 2011. Network environ theory,
#' simulation and EcoNet 2.0. Environ. Model. Softw. 26, 419-428.
#'
#' Szyrmer, J., Ulanowicz, R. E., 1986. "Total Flows in Ecosystems". Ecol. Mod. 35:123-136.
#'
#' Ulanowicz, R.E., 2004. Quantitative methods for ecological network analysis.
#' Comput. Biol. Chem. 28, 321-33
#'
#' Ulanowicz, R.E., Holt, R.D., Barfield, M., 2014. Limits on ecosystem trophic
#' complexity: insights from ecological network analysis.  Ecology Letters
#' 17:127-136.
#' @examples
#'
#'
#'
#' data(troModels)
#' F = enaFlow(troModels[[6]])  # completes the full analysis
#' F$ns  # returns just the network statisics
#'
#'
#'
#' @importFrom MASS ginv
#' @import network
#' @importFrom stats sd
#' @export enaFlow
enaFlow <- function(x,zero.na=TRUE,balance.override=FALSE){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }

                                        # unpack model
  Flow <- t(as.matrix(x,attrname = 'flow')) #flows
  input <- x%v%'input' #inputs
  stor <- x%v%'storage' #storage values

  n <- nrow(Flow)      # number of nodes
  I <- diag(1,nrow(Flow),ncol(Flow))          # create identity matrix
  T. <- apply(Flow,1,sum) + input;   # input throughflow (assuming steady state)

                                        #compute the intercompartmental flows
  GP <- Flow / T.  #Input perspective
  G <- t(t(Flow) / T.)  #Output perspective

                                        #check and replace NA values with 0 if zero.na
  if (zero.na){
    GP[is.na(GP)] <- 0
    G[is.na(G)] <- 0
    GP[is.infinite(GP)] <- 0
    G[is.infinite(G)] <- 0
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


  ## Total Flows (Szyrmer & Ulanowicz 1987) as implimented in Kay, Graham, & Ulanowicz 1989 "A detailed guide to netwokr analysis"

  TF.in <- (NP-I) %*% ginv(diag(diag(NP))) %*% diag(T.) # total flows - inputs
  TCC <- ginv(diag(T.)) %*% TF.in                       # total contribution coefficients

  TF.out <-(N-I) %*% ginv(diag(diag(N))) %*% diag(T.)   # total flows - ouptuts
  TDC <- ginv(diag(T.)) %*% TF.out

  ## Network Statistics ---------------------------------------------
  TST <- sum(T.)  # total system throughflow
  TSTp <- sum(Flow) + sum(x%v%'input') + sum(x%v%'output') # total system throughput

  Boundary <- sum(input)
  APL <- TST/Boundary  # Average Path Lenght (Finn 1976; aka network
                      # aggradation, multiplier effect)

                                        # Finn Cycling Index
  p <- as.matrix(rep(1,n),nrow=n)
  dN <- diag(N)
  TSTc <- sum((dN-p)/dN *T.)
  FCI <- TSTc/TST

                                        # non-locality (realized)
  direct <- sum(G %*% input)
  indirect <- sum((N - I - G) %*% input)
  ID.F <- indirect/direct
  BFI <- Boundary/TST
  DFI <- sum(G %*% input) / TST
  IFI <- indirect/TST
                                        # non-locality (idealized)
  ID.F.O <- sum(N-I-G)/sum(G)
  ID.F.I <- sum(NP-I-GP)/sum(GP)
                                        # HMG
  HMG.O <- ( sd(as.vector(G)) / mean(G) ) / ( sd(as.vector(N)) / mean(N) )
  HMG.I <- ( sd(as.vector(GP)) / mean(GP) ) / ( sd(as.vector(NP)) / mean(NP) )
                                        # Amplification
  AMP.O <- length(which( (N - diag(diag(N))) > 1))
  AMP.I <- length(which( (NP - diag(diag(NP))) > 1))
                                        # MODE ANALYSIS
                                        # This is built from Fath's original MODE program
  mode0.F <- Boundary                     # boundary flow
  mode1.F <- sum(ginv(diag(diag(N))) %*% N %*% diag(input) - diag(as.vector(I%*%input))) # internal first passage flow
  mode2.F <- sum((diag(diag(N))-I) %*% ginv(diag(diag(N))) %*% N %*% diag(input))  # cycled flow
  mode3.F <- mode1.F                      # dissipative equivalent to mode 1
  mode4.F <- mode0.F                    # dissipative equivalent to mode 0
                                        #re-orientation
  orient <- get.orient()
  if (orient == 'rc'){
      G <- t(G)
      GP <- t(GP)
      N <- t(N)
      NP <- t(NP)
      TCC <- t(TCC)
      TDC <- t(TDC)
  }


  asc <- enaAscendency(x)
                                        #network statistics
  ns <- cbind(Boundary,TST,TSTp,APL,FCI,
              BFI,DFI,IFI,
              ID.F,ID.F.I,ID.F.O,
              HMG.I,HMG.O,
              AMP.I,AMP.O,
              mode0.F,mode1.F,mode2.F,mode3.F,mode4.F, asc)

                                        #output
  return(list('T'=T.,'G'=G,'GP'=GP,'N'=N,'NP'=NP, 'TCC'=TCC, 'TDC'=TDC, 'ns'=ns))
}


