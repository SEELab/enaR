#' get.ns.R
#' Input = network model
#' Output = a vector of global network statistics from ena
#'
#' Borrett | July 4, 2012
#' -----------------------------------







#' get.ns.R Input = network model Output = a vector of global network
#' statistics from ena
#'
#' Borrett | July 4, 2012 ----------------------------------- get.ns.R Input =
#' network model Output = a vector of global network statistics from ena
#'
#' Borrett | July 4, 2012 ----------------------------------- Quick Calculation
#' of a Range of Network Statistics.
#'
#' This is a high level function for calculated the main network analyses
#' (Ascendancy, Flow, Structure, Storage and Utility) on an ecological network.
#'
#' @param x A network object.
#' @param balance.override Turns off balancing and balance checking.
#' @return Returns the network statistics (ns) of all of the major ENA
#' functions: enaStructure, enaFlow, enaAscendency, enaStorage and enaUtility
#' (both flow and storage).
#' @author Matthew K. Lau Stuart R. Borrett David E. Hines
#' @seealso
#' \code{\link{enaStructure}},\code{\link{enaFlow}},\code{\link{enaAscendency}},\code{\link{enaUtility}}
#' @references Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
#' Environ Analysis.  Environ. Model. Softw. 21, 375-405.
#' @examples
#'
#'
#'
#' data(troModels)
#' get.ns(troModels[[6]])
#'
#'
#' @import network
#' @export get.ns
get.ns <- function(x,balance.override=FALSE){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }
  # runs selected ena analyses that return global network statistics
  st <- enaStructure(x)$ns
  Flow <- enaFlow(x)$ns
  asc <- enaAscendency(x)
  s <- enaStorage(x)$ns
  u.f <- enaUtility(x,type='flow',eigen.check=FALSE)$ns
  u.s <- enaUtility(x,type='storage',eigen.check=FALSE)$ns
  ns <- data.frame(st,Flow,asc,s,u.f,u.s)
  rownames(ns) <- ""
  return(ns)
}


