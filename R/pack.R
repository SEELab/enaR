#' Compile Network Information into a Network Class
#'
#' This function provides a flexible framework for importing flow network
#' information into a network class object for analyses.
#'
#' @param flow The flow matrix.
#' @param input The inputs into the system.
#' @param respiration The quantities respired from the system.
#' @param export The exports from the system.
#' @param output The output (i.e. exports + respiration) from the system.
#' @param storage The quantities stored in compartments within the system.
#' @param living A logical vector indicating whether a node is either
#' 'living' (= TRUE) or 'dead' (=FALSE).
#' @return Returns a network object for the supplied model.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{unpack}}
#' @import network
#' @export pack
pack <- function(flow,
                 input = NA,
                 respiration = NA,
                 export = NA,
                 output = NA,
                 storage = NA,
                 living=NA){
                                        #Warn if missing both
  if (all(is.na(respiration)) & all(is.na(export))){
    warning('Missing or NA resipiration/export values.')
  }else if (any(is.na(output) == FALSE) &
            any(is.na(export) == FALSE)){
                respiration <- output - export
            }else if (any(is.na(output) == FALSE) &
                      any(is.na(respiration) == FALSE)){
                export <- output - respiration
            }else if (all(is.na(output)) & any(c(is.na(respiration), is.na(export)) == FALSE)){
                export. <- export
                respiration. <- respiration
                export.[is.na(export)] <- 0
                respiration.[is.na(respiration)] <- 0
                output <- export. + respiration.
                output[(is.na(export) + is.na(respiration)) == 2] <- NA
            }
                                        #Add rownames
  if (length(rownames(flow))==0){rownames(flow) <- colnames(flow) <- as.character(1:nrow(flow))}
                                        #Compiling the objects into a list
  x <- list('flow' = as.matrix(flow),'input' = input,'export' = export,
            'respiration' = respiration, 'storage' = storage,'living'=living)

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
  y <- network(x[[1]],directed=TRUE,loops=TRUE)
  # edge
  set.edge.attribute(y,names(x)[1],as.numeric(x[[1]]))
                                        #packing up the attributes into the network object (y)
  flow <- as.matrix(flow)
  rownames(flow) <- colnames(flow)
  set.edge.attribute(y,'flow',flow[flow>0])
  # vertex
  set.vertex.attribute(y,'input',input)
  set.vertex.attribute(y,'export',export)
  set.vertex.attribute(y,'respiration',respiration)
  set.vertex.attribute(y,'output',output)
  set.vertex.attribute(y,'storage',storage)
  set.vertex.attribute(y,'living',living)
  set.vertex.attribute(y,'vertex.names',rownames(flow))
                                        #naming the rows and columns of the flow matrix and storing
                                        #it in the network attributes



                                        #check if model is balanced
  y %n% 'balanced' <- ssCheck(y) #check if the model is balanced

  return(y)
}
