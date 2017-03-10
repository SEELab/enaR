#' TES.R  --- TOTAL ENVIRON STORAGE
#' INPUT = network model
#' OUTPUT = total environ throughput - unit and scaled
#'
#' Borrett | July 7, 2012
#' ---------------------------------------------------







#' TES.R --- TOTAL ENVIRON STORAGE INPUT = network model OUTPUT = total environ
#' throughput - unit and scaled
#' 
#' Borrett | July 7, 2012 ---------------------------------------------------
#' TES.R --- TOTAL ENVIRON STORAGE INPUT = network model OUTPUT = total environ
#' throughput - unit and scaled
#' 
#' Borrett | July 7, 2012 ---------------------------------------------------
#' Calculate the Total Environ Storage
#' 
#' Calculates the total storage in each n input and output environs.  This
#' function calculates the storage for both the unit input (output) and the
#' realized input (output) environs.  Realized uses the observed inputs
#' (outputs) rather than an assumed unit input (output) to each node.
#' 
#' @param x A network object.
#' @param balance.override LOGICAL: should balancing being ignored.
#' @return \item{realized.input}{input oriented, realized storage in each
#' environ.} \item{realized.output}{output oriented, realized storage in each
#' environ.} \item{unit.input }{input oriented, unit storage in each environ.}
#' \item{unit.output}{input oriented, unit storage in each environ.}
#' @author Matthew K. Lau Stuart R. Borrett David E. Hines
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' \code{\link{enaStorage},\link{enaEnviron}}
#' @references Matis, J.H. and Patten, B.C. 1981.  Environ analysis of linear
#' compartmenal systems: the static, time invariant case.  Bulletin of the
#' International Statistical Institute. 48, 527--565.
#' @examples
#' 
#' 
#' 
#' data(troModels)
#' tes <- TES(troModels[[6]])
#' tes
#' 
#' 
#' 
#' @export TES
#' @import network
TES <- function(x,balance.override=FALSE){

                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' = ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }
                                        #
  oo <- get.orient() #original orientation
  if (oo == 'school'){oo <- 'internal'}
  set.orient('internal')
  S <- enaStorage(x)
  set.orient(oo)
  input <- unpack(x)$z   # get data input elements
  output <- unpack(x)$y  # get data output elements

  # UNIT
  X = S$S 
  unit.output <- apply(X,2,sum)
  X =  S$SP
  unit.input <- apply(X,2,sum)

  # REALIZED
  X = S$S %*% diag(input)
  realized.output <- apply(X,2,sum)
  X =  diag(output) %*% S$SP
  realized.input <- apply(X,2,sum)
  
  return(list("realized.input"=realized.input,"realized.output"=realized.output,"unit.input"=unit.input,"unit.output"=unit.output))
}
