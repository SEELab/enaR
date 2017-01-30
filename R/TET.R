#' TET.R  --- TOTAL ENVIRON THROUGHFLOW
#' INPUT = network model
#' OUTPUT = total environ throughput - unit and scaled
#'
#' Borrett | July 7, 2012
#' ---------------------------------------------------





#' TET.R --- TOTAL ENVIRON THROUGHFLOW INPUT = network model OUTPUT = total
#' environ throughput - unit and scaled
#' 
#' Borrett | July 7, 2012 ---------------------------------------------------
#' Calculates the Total Environ Throughflow for a Ecosystem Network Model
#' 
#' Determines the total environ throughflow (TET) for each of the 2 x n
#' environs of the selected network model. It returns both the TET calculated
#' from a unit input (output) vector and from the observed or realized input
#' (output) vector.
#' 
#' @param x A network object.
#' @param balance.override Logical: should the function work if the model is
#' not at steady-state?
#' @return \item{realized.input}{vector of the n realized total environ
#' throughflows for the n input oriented environs.}
#' \item{realzied.output}{vector of the n realized total environ throughflows
#' for the n ouptut oriented environs.} \item{unit.input}{vector of the n unit
#' total environ throughflows for the n input oriented environs.}
#' \item{unit.output}{vector of the n unit total environ throughflows for the n
#' output oriented environs.}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{enaEnviron}}
#' @references Gattie, D.K., Schramski, J.R., Borrett, S.R., Patten, B.C.,
#' Bata, S.A., and Whipple, S.J. 2006. Indirect effects and distributed control
#' in ecosystems: Network environ analysis of a seven-compartment model of
#' nitrogen flow in the Neuse River Estuary, USA---Steady-state analysis. Ecol.
#' Model. 194:162--177.
#' 
#' Whipple, S.J., Borrett, S.R., Patten, B.C., Gattie, D.K., Schramski, J.R.,
#' and Bata, S.A. 2007.  Indirect effects and distributed control in
#' ecosystems: Comparative network environ analysis of a seven-compartment
#' model of nitrogen flow in the Neuse River Estuary, USA---Time series
#' analysis. Ecol. Model. 206: 1--17.
#' @examples
#' 
#' 
#' data(troModels)
#' tet <- TET(troModels[[6]])
#' tet
#' 
#' 
#' @export TET
TET <- function(x,balance.override=FALSE){

                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' = ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }

  oo <- get.orient() #original orientation
  if (oo == 'school'){oo <- 'internal'}
  set.orient('internal')
  E <- enaEnviron(x)
  set.orient(oo)
  input <- unpack(x)$z   # get data input elements
  output <- unpack(x)$y  # get data output elements

  # UNIT
  unit.input <- 0
  unit.output <- 0

  # REALIZED
  realized.input <- 0 # initialize
  realized.output <- 0 # initialize

  # UNIT & SCALED
  for(i in 1:length(input)){
    realized.input[i] = -sum(diag(E$input[[i]] * output[i]))
    realized.output[i] = -sum(diag(E$output[[i]] * input[i]))
    unit.input[i] = -sum(diag(E$input[[i]]))
    unit.output[i] = -sum(diag(E$output[[i]]))
  }

  return(
         list("realized.input"=realized.input,
              "realized.output"=realized.output,
              "unit.input"=unit.input,
              "unit.output"=unit.output)
         )

}
