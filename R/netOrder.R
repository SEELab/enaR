#'## Function to order the nodes in a Network in enaR
#'## Singh P. | July 2014
#'## -----------------------------------------


#' ## Function to order the nodes in a Network in enaR ## Singh P. | July 2014
#' ## ----------------------------------------- ## Function to order the nodes
#' in a Network in enaR ## Singh P. | July 2014 ##
#' ----------------------------------------- Reorder Nodes in a Network in enaR
#' 
#' Reorders nodes in a network either through a user defined node order vector
#' or by default places the non-living nodes to the end of the node vector,
#' minimizing the order change for other nodes.
#' 
#' 
#' @param x A network object. This includes all weighted flows into and out of
#' each node.
#' @param order An integer vector of length N, where N is number of nodes in x,
#' specifying the new order of the nodes (by default order = 0, which indicates
#' moving non-living nodes to the end)
#' @return Returns a network object with nodes ordered as per the node order
#' vector or without the node order vector, by default moves the non-living
#' nodes to the end of the node vector, minimizing the order change for other
#' nodes.
#' @note The node order vector "order" must be of length equal to the number of
#' nodes in x (i.e. N) and must contain all integers from 1 to N.
#' 
#' This function can be used with default conditions (i.e. without "order"
#' vector) to reorder the nodes of a network which does not have non-living
#' nodes placed at the end so that the Trophic Aggregations analysis
#' (enaTroAgg) can be run on the reordered model.
#' @author Pawandeep Singh
#' @seealso \code{\link{enaTroAgg}}
#' @examples
#' 
#' 
#' 
#' data(troModels)
#' new.network <- netOrder(troModels[[6]], c(1, 3, 2, 5, 4))
#' # new.network is the required rearranged network with nodes in the desired order.
#' 
#' 
#' 
#' @export netOrder
#' @import network
netOrder <- function(x = 'network object',order = 0) {
    if (class(x) != "network") {
        stop("x is not a network class object")
    }
                                        # Load Initials
    flow <- as.matrix(x, attrname = "flow")
   # input <- x %v% "input"
   # resp <- x %v% "respiration"
   # export <- x %v% "export"
   # output <- x %v% "output"
   # storage <- x %v% "storage"
    living <- x %v% "living"
   # names <- x %v% "vertex.names"
    N <- length(living)
                                        # Determine Order (ordr)

    if(identical(order,0)==TRUE) {
        order<-rep(0,N)
        liv1<-which(living==TRUE)
        liv2<-which(living==FALSE)
        order<-c(liv1,liv2)
        if(identical(order,1:N)==TRUE) {warning('Network meets default conditions, no changes made')}
    }



                                        # Rearrange Network Characteristics
   # living <- living[ordr]
    flow  <- flow[order,order]
   # export <- export[ordr]
   # resp <- resp[ordr]
   # storage <- storage[ordr]
   # output <- output[ordr]
   # input <- input[ordr]
   # names <- names[ordr]


                                        # Modify Network
    x<-permute.vertexIDs(x,order)
    set.edge.attribute(x, 'flow', flow[flow>0])
    #x %v% "input" <- input
    #x %v% "respiration" <- resp
    #x %v% "export" <- export
    #x %v% "output" <- output
    #x %v% "storage" <- storage
    #x %v% "living" <- living
    #x %v% "vertex.names" <- names


                                        # Return the ordered network
    return(x)

}
