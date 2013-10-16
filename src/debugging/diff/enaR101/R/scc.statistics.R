# scc.statistics --- structural statistics
# of the strongly connected component
# INPUT = an adjacency matrix
# OUTPUT = structural statistics
# S. Borrett | July 2011
# ------------------------------------

scc.statistics <- function(A='adjacency'){
                                        #Check for network class
  if (class(A) != 'matrix'){warning('A is not a matrix class object')}

  y <- scc(A)   # run scc function on the moodel
  
  nn <- length(y$scc.id)
  K <- list()
                                        # find structural statistics for subgraphs
  if(length(y$scc.id) == 0){
    K[[1]] <- structure.statistics(A)
    rownames(K[[1]]) <- A
  } else {
      for(i in 1:nn){
        j <- which(y$membership==y$scc.id[i])
        K[[i]] <- structure.statistics(A[j,j])
        rownames(K[[i]]) <- paste("K",as.character(i),sep="")
      }
    }
  return("K"=K)
}
