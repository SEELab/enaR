% Please edit documentation in R/unpack.R
\name{unpack}
\alias{unpack}
\title{unpack --- extracts network object into
a list
INPUT = network object
OUTPUT = list of network model components
S. Borrett and M. Lau | July 2011
------------------------------------
unpack --- extracts network object into a list INPUT = network object OUTPUT
= list of network model components S. Borrett and M. Lau | July 2011
------------------------------------ unpack --- extracts network object into
a list INPUT = network object OUTPUT = list of network model components S.
Borrett and M. Lau | July 2011 ------------------------------------
"Unpacks" the Network Object into Separate Objects}
\usage{
unpack(x = "network object")
}
\arguments{
\item{x}{A network object.  This includes all weighted flows into and out of
each node.}
}
\value{
\item{F}{matrix of flows from each node to each node oreinted row to
column.} \item{z}{Node boundary inputs.} \item{r}{Node boundary loss from
respiration.} \item{e}{Node boundary loss due to exportation} \item{y}{Node
boundary loss; summation of r and e} \item{X}{Node storage or biomass}
\item{living}{Logical vector indicating whether each node is living or not}
}
\description{
Separates the components of a network object into separate components within
a list. This includes inputs, exports, respirations, outputs (exports +
respirations), storage, and internal flows.
}
\note{
Flows are oriented from row to column.
}
\examples{



data(troModels)
unpack(troModels[[6]])



}
\author{
Matthew K. Lau Stuart R. Borrett
}
\seealso{
\code{\link{pack},\link{read.scor}}
}
