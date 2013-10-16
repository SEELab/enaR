# pack --- helper function for inputing flow 
# network information into a network object
# INPUT = flow network model components
# OUTPUT = a network object
# M.Lau | July 2011
# ------------------------------------

pack <- function(flow,input,export=0,respiration=0,storage=0,living=TRUE,use.output=FALSE){
                                        #makes export and respiration 0 by default and output the sum
                                        #of export and respiration for each node if output == TRUE
  if (all(is.na(export))){export = NA}else if (length(export) == 1 & export[1] == 0){export <- rep(0,nrow(flow))}
  if (all(is.na(respiration))){
    respiration <- NA}else if (length(respiration) == 1 & respiration[1] == 0){
      respiration <- rep(0,nrow(flow))}
  if (any(is.na(export)) & any(is.na(respiration))){
    output <- NA
  }else if (any(is.na(export)) == TRUE & any(is.na(respiration)) == FALSE){
    output <- respiration
  }else if (any(is.na(export)) == FALSE & any(is.na(respiration)) == TRUE){
    output <- export
  }else {output <- export + respiration}
  if (all(storage == 0)){storage = rep(0,nrow(flow))}
  if (use.output[1] == TRUE){respiration <- export <- NA}
  if (length(living) == 1){living <- rep(TRUE,nrow(flow))}else{} #DEFAULT = all nodes living

                                        #compiling the objects into a list
  x <- list('flow' = as.matrix(flow),'input' = input,'export' = export,
    'respiration' = respiration, 'storage' = storage,'output'=output,'living'=living)
  
                                        #initializing the network object using the flow matrix
  y <- network(x[[1]],directed=TRUE)
  set.edge.attribute(y,names(x)[1],as.numeric(x[[1]]))

                                        #packing up the attributes into the network object (y)
  set.vertex.attribute(y,'input',input)
  set.vertex.attribute(y,'export',export)
  set.vertex.attribute(y,'respiration',respiration)
  set.vertex.attribute(y,'storage',storage)
  set.vertex.attribute(y,'output',output)
  set.vertex.attribute(y,'living',living)

                                        #naming the rows and columns of the flow matrix and storing 
                                        #it in the network attributes
  rownames(flow) <- colnames(flow)
  y%n%'flow' <- as.matrix(flow)

                                        #storing the vertex names in the vertex attributes
  ## y%v%'vertex.names'=as.character(sapply(y%v%'vertex.names',rename))	

  y%n%'balanced' <- ssCheck(y) #check if the model is balanced

  return(y)

}
