#' force.balance --- repeatedly applies balance until 
#' sub-tolerance is reached
#' INPUT = network model
#' OUTPUT = balanced model
#' M. Lau 1 Oct 2012
#' ---------------------------------------------------



#' Repeated Application the Balance Function
#' 
#' This function repeatedly balances a model, sequentially with the output
#' being passed back to the balance function, until it is within tolerance or
#' the maximum number of iterations is reached.
#' 
#' 
#' @param x A network object.
#' @param tol Percent error tolerance for difference between inputs and
#' outputs.
#' @param max.itr Maximum number iterations.
#' @param method The balancing method to use, see balance. DEFAULT = AVG2.
#' @return Returns a balanced network model.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{balance}}
#' @references Allesina, S., Bondavalli, C., 2003.Steady state of ecosystem
#' flow networks: a comparison between balancing procedures.Ecological
#' Modelling 165(2-3):231-239.
#' @examples
#' 
#' data(troModels)
#' ssCheck(troModels[[1]])
#' fb.model=force.balance(troModels[[2]]) #produces a balanced model
#' 
#' @export force.balance
force.balance <- function(x,tol=5,max.itr=10,method='AVG2'){
  n.itr <- 1 # initiate counter
  while(ssCheck(x)==FALSE & n.itr<max.itr){
    x <- balance(x,method=method,tol=tol)
    n.itr <- n.itr + 1
  }
  if (n.itr>=max.itr){
    warning('Maximum iterations reached.')
  }else{
    return(x)
  }
}
