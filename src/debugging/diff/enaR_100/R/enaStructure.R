# enaStructure --- performes strucutral analysis of the 
# network graph (see Borrett et al. 2007)
# INPUT = network object
# OUTPUT = list of structure statistics
# 
# S. Borrett and M. Lau | March 2011
# ---------------------------------------------------

enaStructure <- function(x = 'network object',balance.override=FALSE){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Check for balancing
  if (balance.override == TRUE){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }
  
  F <- t(x%n%'flow')
  A <- sign(F)   # get adjacency matrix
  sp <- structure.statistics(A)    # calls structure.statistics helper function
  
  return(list('A'=A,'ns'=sp))  # "A" is the adjacency matrix oriented
                                        # column to row and "sp" is a list of
                                        # structural network staistics
}
