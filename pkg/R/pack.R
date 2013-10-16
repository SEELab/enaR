# pack --- helper function for inputing flow 
# network information into a network object
# INPUT = flow network model components
# OUTPUT = a network object
# M.Lau | July 2011
# ------------------------------------

pack <- function(flow,input=NA,output=NA,export=NA,respiration=NA,storage=NA,living=NA){
                                        #check for igraph
  det.igraph <- any(search()=="package:igraph")
  if (det.igraph){
    warning('Function conflicts with igraph. Use detach(package:igraph) to detach the igraph package.')
  }else{}
                                        #Create output from export and respiration
  if (all(is.na(output))){
    if (length(export)==nrow(flow)&length(respiration)==nrow(flow)){
      output <- export + respiration
    }else if (length(export)==nrow(flow)&length(respiration)!=nrow(flow)){
      output <- export
    }else if (length(export)!=nrow(flow)&length(respiration)==nrow(flow)){
      output <- respiration
    }
  }
                                        #Compiling the objects into a list
  x <- list('flow' = as.matrix(flow),'input' = input,'export' = export,
            'respiration' = respiration, 'storage' = storage,'output'=output,'living'=living)
   
                                        #Warning for missing components
  if(any(is.na(unlist(x)))){
    missing <- print(names(unlist(x))[is.na(unlist(x))])
    if (length(missing)>1){
      for (i in 2:length(missing)){
        missing[1] <- paste(missing[1],missing[i],sep=', ')
      }
    }
    warning(paste('Missing model components:',missing[1],sep=' '))
  }else{}
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
  set.vertex.attribute(y,'vertex.names',rownames(flow))
                                        #naming the rows and columns of the flow matrix and storing 
                                        #it in the network attributes
  rownames(flow) <- colnames(flow)
  y%n%'flow' <- as.matrix(flow)
                                        #check if model is balanced
  y%n%'balanced' <- ssCheck(y) #check if the model is balanced
  return(y)
}
