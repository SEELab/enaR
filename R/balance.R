#' Balance Flow Network Models
#' 
#' Applies the methods of Allesina and Bondavalli (2003) for balancing flow
#' network models.
#' 
#' 
#' @param x A network object.
#' @param method Methods for model balancing, see Allesina and Bondavalli
#' (2003).
#' @param tol Percent error tolerance used in the steady state check prior to
#' balancing.
#' @return Returns a network object with a balanced flow network model.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{bal}}
#' @references Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem
#' flow networks: a comparison between balancing procedures. Ecological
#' Modelling 165(2-3):231-239.
#' @examples
#' 
#' 
#' 
#' data(troModels)
#' balance(troModels[[6]])
#' 
#' 
#' 
#' @import network
#' @export balance
balance <- function(x,method=c('AVG2','AVG','IO','OI','I','O'),tol=5){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
  eT <- as.extended(x) #convert to extended format
  n <- network.size(x)
                                        #checks
  check <- ssCheck(x,tol)
  if (check){
    print('BALANCED',quote=FALSE);
    x%n%'balanced' = TRUE;
    return(x);
    stop
  }else{
    method <- method[1]
    print(method,quote= FALSE)
                                        #balancing
    if (method == 'AVG'){  ##Using the AVG method
      T.bal = 0.5 * (bal(eT,'input') + bal(eT,'output'))
    }else if (method == 'AVG2'){   ##Using the AVG2 method
      T.bal <- 0.5 *  (bal((0.5 * bal(eT,'output') + 0.5 * eT),'input')
                       + bal((0.5 * bal(eT,'input') + 0.5 * eT),'output'))
    }else if (method == 'IO'){   ##Using the IO method
      T.bal <- bal((0.5 * bal(eT,'input') + 0.5 * eT),'output')
    }else if (method == 'OI'){   ##Using the OI method
      T.bal <- bal((0.5 * bal(eT,'output') + 0.5 * eT),'input')
    }else if (method == 'I'){  # using the Input method
      T.bal <- bal(eT,'input')
    }else if (method == 'O'){
      T.bal <- bal(eT,'output')
    }else {warning('Unknown balancing method')}
                                        #convert balanced model into network class
    f <- T.bal[1:n,1:n] # create flow matrix
    set.edge.attribute(x,'flow',f[f>0])  # add flow weights to network edges
    x%v%'input' <- T.bal[(n+3),1:n]
    x%v%'export' <- T.bal[1:n,(n+1)]
    x%v%'respiration' <- T.bal[1:n,(n+2)]
    x%v%'output' <- (x%v%'export' + x%v%'respiration')
    x%v%'storage' <- x%v%'storage'
                                        #check for balancing and return output
    if (ssCheck(x)){
      x%n%'balanced' <- TRUE
      return(x)
    }else{
                                        #return false for unbalanced models
      warning('Model was not balanced.')}
      x%n%'balanced' <- FALSE
      return(x)
  }

}
