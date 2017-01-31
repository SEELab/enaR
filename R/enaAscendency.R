#' enaAscendency --- calculates the ascendency statistics
#' of Ulanowicz
#' INPUT = network object
#' OUTPUT = matrix of ascendency statistics
#'
#' D. Hines | December 2011
#' S.R. Borrett | May 2016 - updates
#' ---------------------------------------------------







#' enaAscendency --- calculates the ascendency statistics of Ulanowicz INPUT =
#' network object OUTPUT = matrix of ascendency statistics
#' 
#' D. Hines | December 2011 S.R. Borrett | May 2016 - updates
#' --------------------------------------------------- enaAscendency ---
#' calculates the ascendency statistics of Ulanowicz INPUT = network object
#' OUTPUT = matrix of ascendency statistics
#' 
#' D. Hines | December 2011 S.R. Borrett | May 2016 - updates
#' --------------------------------------------------- Calculates the
#' Ascendency of an Ecological Network
#' 
#' Calculates the average mutual information (AMI), ascendency, overhead, and
#' capacity of input-output networks.  It also returns the ratios of ascendency
#' and overhead to capacity. These metrics describe the organization of flow in
#' an ecological network (Ulanowicz 1997).
#' 
#' @param x A network object.
#' @return \item{H}{Total flow diversity (MacArthur 1955).  Uses the Shannon
#' Information measure (aka Boltzmann entropy) applied to the individual flows.
#' } \item{AMI}{Returns the Average Mutual Information (AMI) in a network. AMI
#' provides a measure of the constraints placed on a given peice of energy
#' matter moving through a network (Patricio et al. 2006) } \item{Hr}{Residual
#' uncertainty that remains about the flow distribution once the ecosystem
#' structure is specified (Hr = H - AMI). } \item{ASC}{Returns the ascendnecy
#' of a network.  Ascendency is a scaled form of AMI relative to the total
#' system throughput (Ulanowicz 1997; 2004).  Total system throughput is the
#' sum of all activity in a network (Kay et al. 1989).} \item{OH}{Returns the
#' overhead of a network.  Overhead is the proportion of the capacity in a
#' network that is not used as ascendency (Ulanowicz 2004).} \item{CAP}{Returns
#' the capacity of a network.  Capacity is defined as the sum of ascendency and
#' overhead (Ulanowicz 2004).} \item{ACS.CAP}{Returns the proportion of
#' capacity used by ascendency.} \item{OH.CAP}{Returns the proportion of
#' capacity used by overhead.} \item{robustness}{Returns the robustness of the
#' network.} \item{ELD}{Returns the Effective Link Density of the network(c)
#' (Ulanowicz et al. 2014).} \item{TD}{Returns the Trophic Depth of the
#' network(r) (Ulanowicz et al. 2014).} \item{A.input}{Returns the input
#' ascendnecy of a network.} \item{A.internal}{Returns the internal ascendnecy
#' of a network.} \item{A.export}{Returns the export ascendnecy of a network.}
#' \item{A.respiration}{Returns the respiration ascendnecy of a network.}
#' \item{OH.input}{Returns the input overhead of a network.}
#' \item{OH.internal}{Returns the internal overhead of a network.}
#' \item{OH.export}{Returns the export overhead of a network.}
#' \item{OH.respiration}{Returns the respiration overhead of a network.}
#' \item{CAP.input}{Returns the input capacity of a network.}
#' \item{CAP.internal}{Returns the internal capacity of a network.}
#' \item{CAP.export}{Returns the export capacity of a network.}
#' \item{CAP.respiration}{Returns the respiration capacity of a network.}
#' @note This and other Ulanowicz school functions require that export and
#' respiration components of output be separately quantified.
#' @author David E. Hines Matthew K. Lau Stuart R. Borrett
#' @seealso
#' \code{\link{read.scor},\link{read.wand},\link{enaStorage},\link{enaUtility}}
#' @references Kay, J.J., Graham, L.A., Ulanowicz, R.E., 1989. A detailed guide
#' to network analysis. p. 15-61 In: Wulff, F., Field, J.G., Man, K.H. (eds.)
#' Network analysis in marine ecology. Coastal Estuarine Study Serries.
#' Springer-Verlag, Berlin.
#' 
#' Patrico, J., Ulanowicz, R.E., Pardal, M.A., Marques J.C., 2004. Ascendency
#' as an ecological indicator: a case study of estuarine pulse eutrophication.
#' Estuar. Coast Shelf S. 60, 23-35.
#' 
#' Ulanowicz, R.E. and Norden, J.S., 1990. Symmetrical overhead in flow
#' networks. International Journal of Systems Science, 21(2), pp.429-437.
#' 
#' Ulanowicz, R.E., 1997. Ecology, The Ascendent Perspective. Columbia
#' University Press, New York.
#' 
#' Ulanowicz, R.E., 2004. Quantitative methods for ecological network analysis.
#' Comput. Biol. Chem. 28, 321-33
#' 
#' Ulanowicz, R.E., Holt, R.D., Barfield, M., 2014. Limits on ecosystem trophic
#' complexity: insights from ecological network analysis. Ecology Letters
#' 17:127-136
#' @examples
#' 
#' 
#' 
#' data(troModels)
#' enaAscendency(troModels[[6]])
#' ####### set initial conditions for calculations #########
#' ## calculate H & CAPACITY  #######################################
#' H = Total Flow Diversity
#' ################### calculate AMI  #######################
#' AMI = Average Mutual Informaiton
#' ################ calculate ascendency ###################
#' ################ calculate residual diversity  ####################
#' ################ calculate overhead  ####################
#' ################### calculate ratios ####################
#' #####################################################################
#' #####################################################################
#' 
enaAscendency <- function(x='network object'){
    if (class(x) != 'network'){warning('x is not a network class object')}


    if (any(is.na(x%v%'export'))){
        warning('Export data is absent from the model.')
    }
    if(any(is.na(x%v%'respiration'))){
           warning('Respiration data is absent from the model.')
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
