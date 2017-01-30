% Please edit documentation in R/netOrder.R
\name{netOrder}
\alias{netOrder}
\title{## Function to order the nodes in a Network in enaR
## Singh P. | July 2014
## -----------------------------------------
## Function to order the nodes in a Network in enaR ## Singh P. | July 2014
## ----------------------------------------- Reorder Nodes in a Network in
enaR}
\usage{
netOrder(x, order = 0)
}
\arguments{
\item{x}{A network object. This includes all weighted flows into and out of
each node.}

\item{order}{An integer vector of length N, where N is number of nodes in x,
specifying the new order of the nodes (by default order = 0, which indicates
moving non-living nodes to the end)}
}
\value{
Returns a network object with nodes ordered as per the node order
vector or without the node order vector, by default moves the non-living
nodes to the end of the node vector, minimizing the order change for other
nodes.
}
\description{
Reorders nodes in a network either through a user defined node order vector
or by default places the non-living nodes to the end of the node vector,
minimizing the order change for other nodes.
}
\note{
The node order vector "order" must be of length equal to the number of
nodes in x (i.e. N) and must contain all integers from 1 to N.

This function can be used with default conditions (i.e. without "order"
vector) to reorder the nodes of a network which does not have non-living
nodes placed at the end so that the Trophic Aggregations analysis
(enaTroAgg) can be run on the reordered model.
}
\examples{


data(troModels)
new.network <- netOrder(troModels[[6]], c(1, 3, 2, 5, 4))
# new.network is the required rearranged network with nodes in the desired order.


}
\author{
Pawandeep Singh
}
\seealso{
\code{\link{enaTroAgg}}
}
