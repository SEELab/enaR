# environ --- conducts environ analysis 
# INPUT = network object
# OUTPUT = input and/or output environs
# 
# M. Lau July 2011 | DEH edited Feb 2013
# ---------------------------------------------------

enaEnviron <- function(x,input=TRUE,output=TRUE,type='unit',err.tol=1e-10,balance.override=FALSE){
                                        #check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Check for balancing
  if (balance.override){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }
                                        #Assume 'rc' orientation of flows
                                        #Don't transpose flows for calculations, they will be transposed in enaFlow
                                        #calculate enaFlow with RC input
  user.orient <- get.orient()
  set.orient('rc')
  F <- enaFlow(x)
  set.orient(user.orient)
                                        #Transpose for calculations in Patten school
  F$G <- t(F$G)
  F$GP <- t(F$GP)
  F$N <- t(F$N)
  F$NP <- t(F$NP)
                                        #Unit environ calculations
  if (input == TRUE){
                                        #Input perspective
    EP <- list()
    for (i in (1:nrow(F$NP))){
      dNP <- diag(F$NP[i,]) #diagonalized N matrix
      EP[[i]] <- dNP %*% F$GP         # calculate internal environ flows
      EP[[i]] <- EP[[i]] - dNP      # place negative environ throughflows on the principle diagonal
      EP[[i]] <- cbind(EP[[i]],apply((-EP[[i]]),1,sum)) #attach z column
      EP[[i]] <- rbind(EP[[i]],c(apply((-EP[[i]]),2,sum))) #attach y row
      EP[[i]][nrow(EP[[i]]),ncol(EP[[i]])] <- 0 #add zero to bottom right corner to complete matrix
      EP[[i]][abs(EP[[i]]) < err.tol] <- 0 #ignore numerical error
                                        #add labels to matrices
      labels <- c(rownames(F$GP))
      colnames(EP[[i]]) = c(labels, 'z')
      rownames(EP[[i]]) = c(labels, 'y')
    }
                                        #add environ names
    names(EP) <- labels
  }
  if (output == TRUE){
                                        #Output perspective
    E <- list()
    for (i in (1:nrow(F$N))){
      dN <- diag(F$N[,i]) #diagonalized N matrix
      E[[i]] <- F$G %*% dN
      E[[i]] <- E[[i]] - dN
      E[[i]] <- cbind(E[[i]], apply((-E[[i]]), 1, sum))
      E[[i]] <- rbind(E[[i]], c(apply((-E[[i]]), 2, sum)))
      E[[i]][nrow(E[[i]]), ncol(E[[i]])] <- 0 
      E[[i]][abs(E[[i]]) < err.tol] <- 0 
                                        #add labels to matrices
      labels <- c(rownames(F$G))
      colnames(E[[i]]) = c(labels, 'z')
      rownames(E[[i]]) = c(labels, 'y')
    }
                                        #add environ names
    names(E) <- labels
  }
                                        #Realized environ calculations
  if (type == 'realized'){
                                        #Input perspective
    if (input == TRUE){
      for (i in (1:nrow(F$N))){
        EP[[i]] <- EP[[i]]*unpack(x)$y[i] #Construct realized environ
      }
    }
                                        #Output perspective
    if (output == TRUE){
      for (i in (1:nrow(F$N))){
        E[[i]] <- E[[i]]*unpack(x)$z[i]
      }
    }
  }
                                        #Wrap-up output into list  
  if (input == TRUE & output == TRUE){
    out <- list('input' = EP,'output' = E)
  } else if (input == TRUE & output == FALSE){
    out <- EP
  } else if (input == FALSE & output == TRUE){
    out <- E
  }      
  
  if (type != 'unit' && type!= 'realized'){
    print('WARNING: Invalid input in type, input ignored')
  }
                                        #re-orient matrices
  if (user.orient == 'rc'){
    for (i in 1:length(out)){
      for (j in 1:length(out[[i]])){
        out[[i]][[j]] <- t(out[[i]][[j]])
      }
    }
  }else{}
                                        #output
  return(out)
}

