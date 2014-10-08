# pack --- helper function for inputing flow
# network information into a network object
# INPUT = flow network model components
# OUTPUT = a network object
# M.Lau & S.R. Borrett | July 2014
# ------------------------------------

repack <- function(x){
                                        #Warn if missing both
#  if (all(is.na(respiration))&all(is.na(export))){
#    warning('Missing or NA resipiration and export values.')
#  }
                                        #Add rownames
#  if (length(rownames(flow))==0){rownames(flow) <- colnames(flow) <- as.character(1:nrow(flow))}
#                                        #Compiling the objects into a list
#  x <- list('flow' = as.matrix(flow),'input' = input,'export' = export,
#            'respiration' = respiration, 'storage' = storage,'living'=living)

                                        #Warning for missing components
#  if(any(is.na(unlist(x)))){
#    missing <- print(names(unlist(x))[is.na(unlist(x))])
#    if (length(missing)>1){
#      for (i in 2:length(missing)){
#        missing[1] <- paste(missing[1],missing[i],sep=', ')
#      }
#    }
#  }else{}
                                        #initializing the network object using the flow matrix
  f <- x%n%'flow'
  y <- network(f,directed=TRUE,loops=TRUE)

  # Network Attributes
  set.network.attribute(y,'balanced',x %n% 'balanced')

  # edge
  set.edge.attribute(y,'flow',f[f>0])
                                        #packing up the attributes into the network object (y)
  # vertex
  set.vertex.attribute(y,'input',x %v% 'input')
  set.vertex.attribute(y,'export',x %v% 'export')
  set.vertex.attribute(y,'respiration', x %v% 'respiration')
  set.vertex.attribute(y,'output', x %v% 'output')
  set.vertex.attribute(y,'storage', x %v% 'storage')
  set.vertex.attribute(y,'living', x %v% 'living')
  set.vertex.attribute(y,'vertex.names', x %v%'vertex.names')
                                        #naming the rows and columns of the flow matrix and storing
                                        #it in the network attributes

  return(y)
}
