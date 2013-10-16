# TES.R  --- TOTAL ENVIRON STORAGE
# INPUT = network model
# OUTPUT = total environ throughput - unit and scaled
#
# Borrett | July 7, 2012
# ---------------------------------------------------
TES <- function(x='network object',balance.override=FALSE){

                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}

                                        #Check for balancing
  if (balance.override == TRUE){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' = ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }

  S <- enaStorage(x)
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

## SRB 7/9/2012 -- something seems off abotu the unit.input TES

