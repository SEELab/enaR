# enaStructure --- performes strucutral analysis of the 
# network graph (see Borrett et al. 2007)
# INPUT = network object
# OUTPUT = list of structure statistics
# 
# S. Borrett and M. Lau | March 2011
# ---------------------------------------------------

enaStructure <- function(x = 'network object',orient=get('orientation',envir=environment(orient.matrices))){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Global orientation
  if (orient=='rc'){F <- t(x%n%'flow')}else{F <- x%n%'flow'}
  A <- sign(F)   # get adjacency matrix
  sp <- structure.statistics(A)    # calls structure.statistics helper function
                                          #Global orientation
  if (orient=='rc'){A <- t(A)}else{}
  return(list('A'=A,'ns'=sp))  # "A" is the adjacency matrix oriented
                                        # column to row and "sp" is a list of
                                        # structural network staistics
}
